# -*- encoding: utf-8 -*-
#
# Copyright (C) 2016 Tomash Brechko.  All rights reserved.
#
# This file is part of GeekJDict.
#
# GeekJDict is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# GeekJDict is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GeekJDict.  If not, see <http://www.gnu.org/licenses/>.
#
package GeekJDict::CLI;
use parent "GeekJDict::Base";

use v5.16;
use strict;
use warnings qw(all);
use utf8;
use open qw(:std :utf8);


use GeekJDict::Util qw(get_globs globs2regexps);


# Following characters SHOULD be narrow and SHOULD NOT match \p{Ea=A}.
my $item = "∙";
my $separator = "-";


sub new {
    my $class = shift;
    my ($option) = @_;

    # To be able to rollback failed transaction we have to open DB in rw mode.
    my $self = $class->SUPER::new($option->{db}, "rw");

    if ($option->{"data-info"}) {
        $self->print_data_info;
        exit;
    }

    ######## TODO: This code chunk should eventually be removed ########
    {
        my $old = $self->{dbh}->selectrow_array(q{
            SELECT ti
            FROM cangjie
            WHERE ti >= 32
            LIMIT 1
        });
        die("Please redo --update=kanji to bring dictionary database",
            " to new format\n")
            if $old;
    }
    ####################################################################

    $self->{"no-colors"} = $option->{"no-colors"};
    $ENV{ANSI_COLORS_DISABLED} = $option->{"no-colors"};
    $ENV{ANSI_COLORS_ALIASES} = $option->{colors};
    # For environment settings above to take effect we do require+import.
    require Term::ANSIColor;
    Term::ANSIColor->import(qw(color coloralias));
    coloralias("separator", "bright_black") unless coloralias("separator");
    coloralias("writing",   "blue")         unless coloralias("writing");
    coloralias("reading",   "green")        unless coloralias("reading");
    coloralias("tag",       "bright_black") unless coloralias("tag");
    coloralias("text",      "clear")        unless coloralias("text");

    $self->{superscript} = $option->{"large-references"}
        ? [qw(a b c d e f g h i j k l m n o p   r s t u v w x y z)]
        : [qw(ᵃ ᵇ ᶜ ᵈ ᵉ ᶠ ᵍ ʰ ⁱ ʲ ᵏ ˡ ᵐ ⁿ ᵒ ᵖ   ʳ ˢ ᵗ ᵘ ᵛ ʷ ˣ ʸ ᶻ)];

    # --RAW-CONTROL-CHARS makes regexp search work even when what
    # appears to be at the line start is preceded by color command.
    $self->{less} = [qw(less --no-init --RAW-CONTROL-CHARS --ignore-case
                             --quit-if-one-screen)] if -t STDIN;

    use GeekJDict::Grammar;
    $self->{grammar} = GeekJDict::Grammar->new($option->{grammar},
                                               $self->{dbh});

    $self->_init_readline;
    $self->_load_meta;
    $self->_prepare_statements;

    $self->_prepare_pronounce($option->{"pronounce-db"});
    $self->{"pronounce-command"} = $option->{"pronounce-command"};
    $self->{"pronounce-delay"} = $option->{"pronounce-delay"};

    return $self;
}


# Term::ANSIColor :pushpop functions in Perl 5.22 do not work with
# ANSI_COLORS_DISABLED, so re-implement them here (also make them
# simpler for our purposes).
#
# CLEARCOLOR and TOPCOLOR are not in Term::ANSIColor.
my @COLORSTACK;
sub CLEARCOLOR {
    @COLORSTACK = ();
}
sub TOPCOLOR {
    return @COLORSTACK ? $COLORSTACK[-1] : color("reset");
}
sub PUSHCOLOR {
    push @COLORSTACK, $_[0] if $_[0];
    return $_[0];
}
sub POPCOLOR {
    pop @COLORSTACK;
    return TOPCOLOR;
}
sub LOCALCOLOR {
    return join "", @_, TOPCOLOR;
}


my $cangjie_help;


# SQLite aggregate function common_prefix().
package GeekJDict::CLI::CommonPrefix;
sub new { bless \my $self, shift }
sub step {
    if (${$_[0]}) {
        my $l = length(${$_[0]});
        ${$_[0]} = substr(${$_[0]}, 0, --$l)
            while ${$_[0]} ne substr($_[1], 0, $l);
    } else {
        ${$_[0]} = $_[1];
    }
}
sub finalize { ${$_[0]} // "" }
package GeekJDict::CLI;


sub _init_readline {
    my $self = shift;

    my $dbh = $self->{dbh};

    use DBD::SQLite;
    $dbh->sqlite_create_aggregate(
        "common_prefix", 1, "GeekJDict::CLI::CommonPrefix",
        DBD::SQLite::Constants::SQLITE_DETERMINISTIC);
    my $kanji_one = $dbh->prepare(q{
        SELECT ?||char(kc)
        FROM cangjie
        WHERE tx = ?
        ORDER BY ti, kc
        LIMIT 1
        OFFSET ?
    });
    my $kanji_each = $dbh->prepare(q{
        SELECT ?||char(kc)
        FROM cangjie
        WHERE tx GLOB ? AND ti < ?
        ORDER BY tx, ti, kc
    });
    # We need nested query to impose ORDER BY on group_concat().
    my $kanji_all = $dbh->prepare(q{
        SELECT ?||group_concat(char(kc), '')
        FROM (SELECT kc, tx
              FROM cangjie
              WHERE tx GLOB ? AND ti < ?
              ORDER BY tx, ti, kc)
        GROUP BY substr(tx, 1, ?)
    });
    my $kanji_expand = $dbh->prepare(q{
        SELECT ?||tx||' '||char(kc)
        FROM cangjie
        WHERE tx GLOB ? AND ti < ?
        ORDER BY tx, ti, kc
    });
    my $kanji_next = $dbh->prepare(q{
        SELECT ?||(CASE WHEN tx = ? THEN tx||'/ '||group_concat(char(kc), '')
                        WHEN count(*) = 1 THEN tx||' '||char(kc)
                        ELSE common_prefix(tx||'/')||' '||count(*) END)
        FROM (SELECT kc, tx AS tx
              FROM cangjie
              WHERE tx GLOB ? AND ti < ?
              ORDER BY tx, ti, kc)
        GROUP BY substr(tx, 1, ?)
    });

    my $common_len;
    my $complete = sub {
        my ($type, $text, $limit_cangjie) = @_;

        my ($prefix, $glob, $index) =
            $text =~ m{^
                (.*?)
                (\*+|\**(?:(?:[A-WY?]|\[\^?(?:[A-WY](?:-[A-WY])?)+\])\**){1,5})
                (?:|(?:/|(?=[1-9]))(|[1-9]\d*))
            $}x;
        return [] unless defined $prefix;

        $common_len = length($prefix);
        my ($code, $code_len);
        if ($glob !~ /[*?[]/) {
            $code = $glob;
            unless ($index) {
                $code_len = length($code);
                $common_len += $code_len;
                $glob .= "*" unless defined $index;
            }
        } else {
            return [] if defined $index;

            $type = "?" if $type eq "@";
        }

        my $limit = $limit_cangjie ? 16 : 256;

        my $comp;
        if ($index) { # Use variant at a given index.
            $comp = $dbh->selectcol_arrayref($kanji_one, undef, $prefix,
                                             $code, $index - 1);
        } elsif ($type eq "@") { # Complete or list next code alternatives.
            $comp = $dbh->selectcol_arrayref($kanji_next, undef, $prefix, $code,
                                             $glob, $limit, $code_len + 1);
            if (@$comp == 1 && $comp->[0] =~ /(.+) \d| (\D)$/) {
                # We have only one match which is either "prefixCODE 42"
                # or "prefixCODE 字".  So for completion we leave either
                # the code ("prefixCODE") or the character ("prefix字").
                $comp->[0] = $1 // $prefix . $2;
            } elsif (@$comp && $comp->[0] =~ /(.+) (\D\D+)/) {
                # Number immediate alternatives if more than one.
                my $i = 0;
                splice(@$comp, 0, 1, map { ++$i; "$1$i $_" } split "", $2);
            }
        } elsif ($type eq "?") { # List all alternatives (expanded).
            $comp = $dbh->selectcol_arrayref($kanji_expand, undef, $prefix,
                                             $glob, $limit);
            # Number all alternatives if more than one and also append
            # slash to codes that are the prefixes of the following
            # codes.
            if (@$comp > 1) {
                my ($i, $cp) = (0);
                for (my $j = 0; $j < @$comp; ++$j) {
                    my ($c) = $comp->[$j] =~ /^(.+) /;
                    if ($cp) {
                        if ($cp eq $c) {
                            ++$i;
                            $comp->[$j - 1] =~ s| |/$i |;
                        } elsif ($i) {
                            ++$i;
                            $comp->[$j - 1] =~ s| |/$i |;
                            $i = 0;
                        } elsif ($cp eq substr($c, 0, length($cp))) {
                            $comp->[$j - 1] =~ s| |/ |;
                        }
                    }
                    $cp = $c;
                }
                if ($i) {
                    ++$i;
                    $comp->[-1] =~ s| |/$i |;
                }
            } elsif (@$comp) {
                # If match is exact remove space before character.
                ++$common_len if substr($comp->[0], $common_len, 1) eq " ";
            }
        } elsif ($type eq "*") { # Substitute all alternatives.
            $comp = $dbh->selectcol_arrayref($kanji_all, undef, $prefix,
                                             $glob, $limit, $code_len);
        } else { # ($type eq "%") Substitute next alternative (cycling).
            $comp = $dbh->selectcol_arrayref($kanji_each, undef, $prefix,
                                             $glob, $limit);
        }

        # When there's only one alternative readline (6.3) in "?" mode
        # won't call user-supplied display callback, but will show raw
        # text.  Also when there's common prefix for all alternatives
        # (for instance in queries like "A?" or "prefix?B") then
        # readline (6.3) assumes input is modified by insertion of
        # such common prefix (even if it's not the case) and doesn't
        # execute "?" until the user presses TAB the second time.  So
        # we add dummy empty alternative to break both ties.
        push @$comp, "" if $type eq "?" && @$comp;

        return $comp;
    };
    my $display = sub {
        my ($matches, $max, $width) = @_;

        # First element in @$matches is readline substitution.
        my $len = @$matches - 1;
        # Do not count last dummy alternative.
        --$len if $matches->[-1] eq "";

        my $columns = int(($width + 2) / ($max - $common_len + 2)) || 1;
        my $rows = int(($len + $columns - 1) / $columns);

        print "\n";  # Leave input line.
        $self->with_less(sub {
            for (my $r = 1; $r <= $rows; ++$r) {
                for (my $c = 0; $c < $columns; ++$c) {
                    my $i = $c * $rows + $r;
                    last if $i > $len;

                    my $m = $matches->[$i];
                    utf8::decode($m);
                    my $len = $max - length($m);
                    substr($m, 0, $common_len, "");
                    $m =~ s/(?=\d+$)/" " x $len . color("tag")/e
                        or $m =~ s/(?=.$)/" " x ($len - 1) . color("writing")/e;
                    print "  " if $c;
                    print $m, color("reset");
                }
                print "\n";
            }
        });
    };

    $cangjie_help =~ s/([a-z=]+)/color("tag") . $1 . color("reset")/eg;
    $cangjie_help =~ s/(\p{Han}+|\p{CJK_Strokes}+)/color("writing")
                                                   . $1 . color("reset")/eg;

    my $kana_inhibitor = qr/(?:^\s*t\s|(?:^|\s)t:)/;
    my $full_cangjie = qr/^\s*[hkw]\s/;

    use GeekJDict::ReadLine;
    $self->{readline} =
        GeekJDict::ReadLine->new($kana_inhibitor, $full_cangjie,
                                 $complete, $display, $cangjie_help);
}


sub _load_meta {
    my $self = shift;

    # We assume that word_meta has no gaps and first ID is zero.
    $self->{meta} = $self->{dbh}->selectall_arrayref(q{
        SELECT ki, ab, ds
        FROM word_meta
        ORDER BY id
    });
}


sub _prepare_statements {
    my $self = shift;

    my $dbh = $self->{dbh};
    $self->{select_tag_id} = $dbh->prepare(q{
        SELECT id
        FROM word_meta
        WHERE id > 0 AND ki NOT GLOB '@*' AND lower(ab) GLOB ?
    });
    $self->{select_tag} = $dbh->prepare(q{
        SELECT ab, ds
        FROM word_meta
        WHERE id > 0 AND ki NOT GLOB '@*' AND lower(ab) GLOB ?
    });
    $self->{select_word} = $dbh->prepare(q{
        SELECT it, tx, jr, mr
        FROM word
        WHERE id = ?
        ORDER BY it
    });
    $self->{select_kanji} = $dbh->prepare(q{
        SELECT tp, tx
        FROM kanji
        WHERE kc = ?
        ORDER BY tp
    });
    $self->{select_cangjie} = $dbh->prepare(q{
        SELECT ti, tx
        FROM cangjie
        WHERE kc = ?
        ORDER BY ti
    });
    $self->{select_writing} = $dbh->prepare(q{
        SELECT sc
        FROM writing
        WHERE cc = ?
        ORDER BY sn
    });
}


sub _prepare_pronounce {
    my $self = shift;
    my ($pronounce_db) = @_;

    return if $pronounce_db eq "";

    my $dbh = $self->{dbh};
    my $NoK = $dbh->selectrow_array(q{
        SELECT id
        FROM word_meta
        WHERE ab = 'NoK'
    });
    $self->{select_reading} = $dbh->prepare(qq{
        SELECT it, tx, jr
        FROM word
        WHERE id = ? AND it & 7 < 2
            AND (mr IS NULL OR NOT instr(mr, char($NoK)))
        ORDER BY it
    });
    $dbh->do(q{
        ATTACH DATABASE 'file:'||?||'?mode=ro' AS pronounce
    }, undef, $pronounce_db);
    $self->{select_pronunciation} = $dbh->prepare(q{
        SELECT a
        FROM pronounce.pronounce
        WHERE w = ? AND r = ?
    });
}


sub count {
    my $self = shift;
    my ($key, $query) = @_;

    return $self->{dbh}->selectrow_array($self->{"count_$key"}, undef, $query);
}


sub process {
    my $self = shift;
    my ($key, $query, $code) = @_;

    my $selector = $self->{"select_$key"};
    $selector->execute($query);
    my @row = (undef) x $selector->{NUM_OF_FIELDS};
    $selector->bind_columns(\(@row));
    eval {
        $code->(@row) while $selector->fetch;
    };
    $selector->finish;
    die $@ if $@;
}


sub run {
    my $self = shift;

    # Most of the time we are blocked either in readline or in less.
    # For readline we have "q" command or C-d (EOF) key, and less will
    # handle SIGINT itself, so we should ignore it.
    local $SIG{INT} = "IGNORE" if exists $self->{less};

    # Make warnings visible in less (die will kill us anyway).  We also
    # let the color leak because we don't know the context anyway.
    local $SIG{__WARN__} = sub { print color("red"), "WARNING: ", @_ };

    my $readline = $self->{readline};
    while (defined (my $input = $readline->readline)) {
        exit if $input eq "q";
        @$self{qw(height width)} = $readline->get_screen_size;
        if ($input =~ s/^t(?:\s+|$)//) {
            $self->with_less(sub { $self->show_tags($input) });
        } elsif ($input =~ s/^h(?:\s+|$)//) {
            $self->with_less(sub { $self->{readline}->print_history($input) });
        } elsif ($input =~ s/^k(?:\s+|$)//) {
            $self->with_less(sub { $self->lookup_kanji($input) });
        } elsif ($input =~ s/^w(?:\s+|$)//) {
            $self->with_less(qw(--window=-1),
                             sub { $self->show_writing($input) });
        } elsif ($input =~ s/^g(?:\s+|$)//) {
            $self->with_less(sub { $self->show_grammar($input) });
        } elsif ($input =~ s/^p(?:\s+|$)//) {
            my $break;
            local $SIG{INT} = sub { $break = 1 };
            $self->pronounce_words($input, \$break);
        } else {
            $self->with_less(sub { $self->lookup_words($input) });
        }
    }
    print "\n";
}


sub with_less {
    my $self = shift;
    my $code = pop @_;

    my $sigpipe;
    eval {
        my $less;
        if (exists $self->{less}) {
            open($less, "|-", @{$self->{less}}, @_)
                or die "Can't pipe to `@{$self->{less}} @_`: $!";
        } else {
            open($less, ">&STDOUT")
                or die "Can't duplicate STDOUT: !";
        }
        select($less);
        $| = 1;  # Flush every write.
        # Bail out on SIGPIPE.
        local $SIG{PIPE} = sub { die \$sigpipe };
        &$code;
    };
    select(STDOUT);
    local $| = 1;  # Enable temporarily to flush the following color reset.
    print color("reset");
    # Clear color stack.
    CLEARCOLOR;
    # Propagate exception if not SIGPIPE.
    die $@ if $@ && (!ref $@ || $@ != \$sigpipe);
}


sub show_tags {
    my $self = shift;
    my ($input) = @_;

    my @globs = get_globs($input);
    if (@globs) {
        shift @globs if $globs[0] eq "";
    } else {
        @globs = ("*");
    }
    my $format =
        color("tag") . $item . "%-11s" . color("text") . "%s" . color("reset");
    my %seen;
    my @lines;
    foreach my $g (@globs) {
        $self->process(tag => $g => sub {
            my ($ab, $ds) = @_;

            return if $seen{$ab}++;
            my $d = (defined $ds
                     ? $ds
                     : color("tag") . "--- (no description)" . color("reset"));
            my $line = sprintf($format, $ab, $d);
            push @lines, $self->wrap_line($line, 12) . "\n";
        });
    }
    print sort { fc($a) cmp fc($b) } @lines;
}


sub lookup_kanji {
    my $self = shift;
    my ($query) = @_;

    my @kanji = ($query =~ /(\p{Han})/g);
    unless (@kanji) {
        print color("separator"), "No kanji given", color("reset"), "\n";
        return;
    }

    my %seen;
    my $count = @kanji;
    my $index = 0;
    foreach my $k (@kanji) {
        next if $seen{$k}++;
        my $kc = ord $k;
        my @k;
        $self->process(kanji => $kc => sub {
            my ($tp, $tx) = @_;

            $k[$tp]= $tx;
        });
        $self->process(cangjie => $kc => sub {
            my ($ti, $tx) = @_;

            push @{$k[9][($ti >> 3) & 1]}, $tx;
        });
        $self->print_kanji($k, \@k, ++$index, $count);
    }
    $self->print_separator("      END");
}


my @radical;

sub print_kanji {
    my $self = shift;
    my ($k, $kanji, $index, $count) = @_;

    $self->print_separator(sprintf " %6d/%-6d", $index, $count);

    unless (@$kanji) {
        print color("separator"), "$k not found", color("reset"), "\n";
        return;
    }

    my @line = (color("writing") . $item . $k . color("tag"));
    push @line, "Strokes: $kanji->[5]"
        if exists $kanji->[5];
    if (exists $kanji->[4]) {
        my $rad = $radical[$kanji->[4]];
        $rad =~ s/^\Q$k(/self (/ or $rad =~ s/^\Q$k/self/;
        push @line, "Radical: $rad [$kanji->[4]]";
    }
    push @line, "Grade: $kanji->[7]"
        if exists $kanji->[7];
    push @line, "Frequency: $kanji->[6]/2501"
        if exists $kanji->[6];
    my $line = join("  $item", @line) . color("reset");
    print $self->wrap_line($line, 7), "\n";

    @line = ();
    if (exists $kanji->[9]) {
        my $cangjie = "Cangjie:";
        $cangjie .= " " . join(", ", @{$kanji->[9][0]})
            if exists $kanji->[9][0];
        $cangjie .= " (" . join(", ", @{$kanji->[9][1]}) . ")"
            if exists $kanji->[9][1];
        push @line, $cangjie;
    }
    if (@line) {
        my $line = "   "
            . color("tag") . join("  $item", "", @line) . color("reset");
        print $self->wrap_line($line, 7), "\n";
    }

    if (exists $kanji->[8]) {
        my $variants = join "  ", split "", $kanji->[8];
        my $line = "     " . color("tag") . $item . "Variants: "
            . color("writing") . $variants . color("reset");
        print $self->wrap_line($line, 16), "\n";
    }

    my $has_reading = 0;
    my @reading = (音読み => 0, 訓読み => 1, 名乗り => 2);
    while (my ($tag, $i) = splice(@reading, 0, 2)) {
        if (exists $kanji->[$i]) {
            my $reading = $kanji->[$i] =~ s/\t/  $item/gr;
            $reading =~ tr/-/〜/;
            $reading =~ s/\.(\S+)/color("separator") . $item
                                  . color("writing") . $1 . color("reading")/ge;
            my $line = color("tag") . $item . "$tag: "
                . color("reading") . $item . $reading . color("reset");
            print "\n" unless $has_reading++;
            print $self->wrap_line($line, 9), "\n";
        }
    }

    if (exists $kanji->[3]) {
        my $meaning = $kanji->[3] =~ s/\t/  $item/gr;
        my $line = "  " . color("text") . $item . $meaning . color("reset");
        print "\n", $self->wrap_line($line, 2), "\n";
    }
}


sub show_writing {
    my $self = shift;
    my ($query) = @_;

    if ($query eq "") {
        print color("separator"), "No character given", color("reset"), "\n";
        return;
    }

    my $c = substr($query, 0, 1);
    my @sc;
    $self->process(writing => ord $c => sub {
        my @c = map { $_ & 1 ? -(($_ >> 1) + 1) : $_ >> 1 } unpack "w*", $_[0];
        $c[0] += 550;
        $c[1] += 550;
        for (my $i = 2; $i < @c; $i += 2) {
            $c[$i] += $c[$i - 2];
            $c[$i + 1] += $c[$i - 1];
        }
        push @sc, \@c;
    });
    unless (@sc) {
        print(color("separator"), "No writing data for $c",
              color("reset"), "\n");
        return;
    }

    my ($width, $height) = ($self->{width}, ($self->{height} - 1) * 2);

    use Imager;
    my $image = Imager->new(xsize => $width, ysize => $height, channels => 1);

    my @palette = ("rgb000", map({ "grey$_" } 0..23), "red");

    my $dim = $height < $width ? $height : $width;
    my $scale = $dim / 1090.;
    my @offset = (($width - $dim) / 2, ($height - $dim) / 2);
    for (my $n = 0; $n < @sc; ++$n) {
        my @points = polybezier(map { $_ * $scale } @{$sc[$n]});
        foreach my $p (@points) {
            $image->circle(x => $p->[0] + $offset[0], y => $p->[1] + $offset[1],
                           r => 1, aa => 1, filled => 1);
        }

        my ($xd, $yd) = (int $points[0][0] + $offset[0],
                         int $points[0][1] + $offset[1]);
        my $frame = "";
        for (my $y = 0; $y < $height; $y += 2) {
            my @f = map { int $_ * .098 } $image->getsamples(y => $y);
            my @b = map { int $_ * .098 } $image->getsamples(y => $y + 1);
            my ($pf, $pb) = (0, 0);
            $frame .= color("$palette[0] on_$palette[0]");
            for (my $x = 0; $x < $width; ++$x) {
                my ($f, $b) = ($f[$x], $b[$x]);
                if ($x == $xd && $y == ($yd & ~1)) {
                    ($yd & 1 ? $b : $f) = -1;
                }
                unless ($self->{"no-colors"}) {
                    my $color = "";
                    if ($pf != $f) {
                        $pf = $f;
                        $color = $palette[$f];
                    }
                    if ($pb != $b) {
                        $pb = $b;
                        $color .= " on_" . $palette[$b];
                    }
                    $frame .= color($color) if $color ne "";
                    $frame .= "▀";
                } else {
                    if ($f == -1 || $b == -1) {
                        $frame .= "X";
                    } elsif ($f > 12 && $b > 12) {
                        $frame .= "█";
                    } elsif ($f > 12) {
                        $frame .= "▀";
                    } elsif ($b > 12) {
                        $frame .= "▄";
                    } else {
                        $frame .= " ";
                    }
                }
            }
            if ($y == 0) {
                my $tag = color("writing on_$palette[0]") . $c . color("tag")
                    . sprintf(" %2d/%-2d", $n + 1, scalar(@sc));
                $frame =~ s/(?:(?:\e\[[^m]*m)*.){8}$/$tag/;
            }
            $frame .= color("reset") . "\n";
        }
        print $frame;
    }
}


sub polybezier {
    sub max { $_[0] > $_[1] ? $_[0] : $_[1] }
    my @points;
    for (my $i = 0; $i < @_ - 2; $i += 6) {
        my $d = (max(abs($_[$i+0] - $_[$i+2]), abs($_[$i+1] - $_[$i+3]))
                 + max(abs($_[$i+2] - $_[$i+4]), abs($_[$i+3] - $_[$i+5]))
                 + max(abs($_[$i+4] - $_[$i+6]), abs($_[$i+5] - $_[$i+7])));
        push @points, [$_[$i+0], $_[$i+1]];
        for (my $n = 1; $n < $d; ++$n) {
            my $t0 = $n / $d;
            my $t1 = 1 - $t0;
            my @c = ($t1*$t1*$t1, 3*$t1*$t1*$t0, 3*$t1*$t0*$t0, $t0*$t0*$t0);
            my $x = ($_[$i+0]*$c[0] + $_[$i+2]*$c[1]
                     + $_[$i+4]*$c[2] + $_[$i+6]*$c[3]);
            my $y = ($_[$i+1]*$c[0] + $_[$i+3]*$c[1]
                     + $_[$i+5]*$c[2] + $_[$i+7]*$c[3]);
            push @points, [$x, $y];
        }
    }
    return @points, [$_[-2], $_[-1]];
}


sub show_grammar {
    my $self = shift;
    my ($query) = @_;

    my $node = $self->{grammar}->infer_backward($query);
    my $word = shift @$node;
    print_node($word, $node, 1) if @$node;

    sub print_node {
        my ($word, $node, $depth) = @_;

        my $indent = " " x $depth;
        if (@{$node->[-1]} == 2) {
            my $n = pop @$node;
            $n->[0] .= " " if $n->[0];
            print($indent, color("separator"), "see $n->[0]",
                  color("reset"), "$word t: $n->[1]\n");
        }
        foreach my $n (@$node) {
            my ($w, $f) = splice @$n, 0, 2;
            $f .= " form" unless $f =~ /\bform\b/;
            print($indent, color("writing"), $word,
                  color("separator"), " is the $f of ", color("writing"), $w,
                  color("reset"), "\n");
            print_node($w, $n, $depth + 1);
        }
    }
}


sub pronounce_words {
    my $self = shift;
    my ($query, $break) = @_;

    unless ($self->{select_pronunciation}) {
        print(color("separator"),
              "Use --pronounce-db option to enable p command",
              color("reset"), "\n");
        return;
    }

    my $ids = $self->find_words($query);

    my $count = @$ids;
    return unless $count;

    my $filter = qr/^(?:@{[ join "|", globs2regexps($query) ]})$/;

    my $index = 0;
    foreach my $id (@$ids) {
        return if $$break;
        my @word = ([], []);
        $self->process(reading => $id => sub {
            my ($it, $tx, $jr) = @_;

            push @{$word[$it & 7]}, [$tx, $jr, $tx =~ /$filter/ ];
        });
        # If globs match only English words then we have enable all Japanese.
        unless (grep { $_->[2] } @{$word[0]}, @{$word[1]}) {
            foreach my $w (@{$word[0]}) {
                $w->[2] = 1;
            }
        }

        $self->print_separator(sprintf " %6d/%-6d", ++$index, $count);
        foreach my $r (@{$word[1]}) {
            my @w = @{$word[0]};
            @w = @w[map { $_ >> 3 } unpack "U*", $r->[1]] if defined $r->[1];
            next unless $r->[2] || grep { $_->[2] } @w;

            if (@w) {
                my $line =  "  " . color("reading") . $r->[0]
                    . "  " . color("writing") . join(", ", map { $_->[0] } @w);
                print $self->wrap_line($line, length($r->[0]) * 2 + 4);
            } else {
                print "  ", color("writing"), $r->[0];
                push @w, [""];
            }

            my $audio;
            my $reading = $r->[0] =~ s/・//gr;
            foreach my $w (@w) {
                $self->{select_pronunciation}->execute($w->[0], $reading);
                $audio = $self->{select_pronunciation}->fetchrow_array;
                last if $audio;
            }
            print color("separator"), "  no audio" unless $audio;
            print color("reset"), "\n";
            if ($audio) {
                select(undef, undef, undef, $self->{"pronounce-delay"});
                open(my $fh, '|-:raw', $self->{"pronounce-command"})
                    or die "Can't run $self->{'pronounce-command'}: $!\n";
                print $fh $audio;
                close($fh);
            }
        }
    }
    $self->print_separator("      END");
}


sub lookup_words {
    my $self = shift;
    my ($query) = @_;

    my $ids = $self->find_words($query);

    my $count = @$ids;
    unless ($count) {
        print color("separator"), "No matches found", color("reset"), "\n";
        return;
    }

    my $index = 0;
    foreach my $id (@$ids) {
        my @word;
        $self->process(word => $id => sub {
            my ($it, $tx, $jr, $mr) = @_;

            push @{$word[$it & 7]}, [$tx, $jr, $mr];
        });
        $self->print_word(\@word, ++$index, $count);
    }
    $self->print_separator("      END");
}


sub find_words {
    my $self = shift;
    my ($query) = @_;

    my ($globs, @tags) = split /(?:^|\s+)t:\s*/, $query;
    $globs = "" unless defined $globs;

    my $dbh = $self->{dbh};

    my @ids;
    my %glob;
    @glob{ map { s/\*\*+/*/gr } get_globs($globs) } = ();
    if (exists $glob{"?*"} || exists $glob{"*?"} || exists $glob{"*?*"}) {
        delete $glob{"?*"};
        delete $glob{"*?"};
        delete $glob{"*?*"};
        $glob{"*"} = undef;
    }
    if (exists $glob{"*"}) {
        if (keys %glob > 1) {
            delete $glob{"*"};
        } else {
            my $count = $dbh->selectrow_array(q{
                SELECT max(id)
                FROM word
            });
            @ids = (1..$count);
        }
    }
    if (keys %glob && !@ids) {
        my @length_eq = grep { /^\?+$/ } keys %glob;
        delete @glob{ @length_eq };
        my @length_ge = grep { /^[*?]+$/ } keys %glob;
        delete @glob{ @length_ge };
        @length_ge = sort map { s/\*//gr } @length_ge;
        # @globs can't contain single quotes or escapes, and we need
        # literal values to enable SQLite to extract common prefix.
        my @globs = map { "glob('$_', tx)" } keys %glob;
        push @globs, map { "length(tx) = " . length $_ } @length_eq;
        push @globs, "length(tx) >= " . length $length_ge[-1] if @length_ge;
        if (@globs > 64) {
            print(color("separator"), "Too many search terms, maximum is 64",
                  color("reset"), "\n");
            return;
        }
        my $shift = 0;
        my $select = $dbh->prepare(qq%
            SELECT @{[ join " | ", map { "(($_)<<@{[ $shift++ ]})" } @globs ]},
                wi
            FROM term_index
            WHERE @{[ join " OR ", @globs ]}
        %);
        $select->bind_columns(\my ($mask, $wi));
        $select->execute;
        my %match;
        while ($select->fetch) {
            my $id = 0;
            foreach my $d (unpack "w*", $wi) {
                $id += $d;
                $match{$id} |= $mask;
            }
        }
        # << operator in Perl is cyclic, so (1 << 64) is 1, not 0.
        my $match = (1 << $#globs) | ((1 << $#globs) - 1);
        while (my ($id, $m) = each %match) {
            push @ids, $id if $m == $match;
        }
        @ids = sort { $a <=> $b } @ids;
    }

    if (@ids && $query =~ /(?:^|\s+)t:/) {
        my @condition;
        foreach my $t (@tags) {
            $t =~ s/\*\*+/*/g;
            my @g = get_globs($t);
            @g = grep { !/^\*$/ && !/^\?\*$/ && !/^\*\?$/ && !/^\*\?\*$/ } @g;
            if (@g) {
                my @c;
                foreach my $g (@g) {
                    $self->process(tag_id => $g => sub {
                        push @c, "instr(mr, char($_[0]))";
                    });
                }
                if (@c) {
                    push @condition, join(" OR ", @c);
                } else {
                    @condition = ();
                    last;
                }
            } else {
                push @condition, undef;
            }
        }
        if (@condition) {
            @condition = grep { defined } @condition;
            if (@condition) {
                my @filtered;
                if (@ids >= 200) {
                    my $filter = $dbh->prepare(qq{
                        SELECT id
                        FROM (SELECT id, group_concat(mr, '') AS mr
                              FROM word
                              WHERE id IN (@{[ "?," x 199 . "?" ]})
                              GROUP BY id)
                        WHERE (@{[ join ") AND (", @condition ]})
                    });
                    do {
                        my @i = splice @ids, 0, 200;
                        my $f = $dbh->selectcol_arrayref($filter, undef, @i);
                        push @filtered, @$f;
                    } while (@ids >= 200);
                }
                if (@ids) {
                    my $filter = $dbh->prepare(qq{
                        SELECT id
                        FROM (SELECT id, group_concat(mr, '') AS mr
                              FROM word
                              WHERE id IN (@{[ join ",", @ids ]})
                              GROUP BY id)
                        WHERE (@{[ join ") AND (", @condition ]})
                    });
                    my $f = $dbh->selectcol_arrayref($filter);
                    push @filtered, @$f;
                }
                @ids = @filtered;
            }
        } else {
            @ids = ();
        }
    }

    return \@ids;
}


sub print_word {
    my $self = shift;
    my ($word, $index, $count) = @_;

    # Replace string of indices with references to auto-vivified
    # scalars that later will hold superscript indices.
    foreach my $e (@{$word->[1]}, @{$word->[2]}) {
        if (defined $e->[1]) {
            my @jr = unpack "U*", $e->[1];
            $e->[1] = [map { \$word->[$_ & 7][$_ >> 3][3] } @jr];
        }
    }

    # Push strings to their corresponding senses.
    for my $i (3..6) {
        if (exists $word->[$i]) {
            foreach my $e (@{$word->[$i]}) {
                my $jr = ord $e->[1];
                push @{$word->[2][$jr >> 3][$i]}, $e;
            }
        }
    }

    $self->print_separator(sprintf " %6d/%-6d", $index, $count);

    my $ss = 0;
    print PUSHCOLOR(color("writing"));
    if ($word->[0]) {
        $ss = $self->print_japanese($word->[0], 0);
        print POPCOLOR, "\n";
        print PUSHCOLOR(color("reading"));
    }
    $self->print_japanese($word->[1], $ss);
    print POPCOLOR, "\n";

    # All readings are writings if there were no writings.
    $ss = @{$word->[1]} unless $word->[0];
    my $i = 0;
    foreach my $e (@{$word->[2]}) {
        print PUSHCOLOR(color("text"));
        $self->print_sense(++$i, $e, $ss);
        print POPCOLOR, "\n";
    }
}


sub print_separator {
    my $self = shift;

    print(color("separator"),
          $separator x ($self->{width} - 14), @_,
          color("reset"), "\n");
}


sub print_japanese {
    my $self = shift;
    my ($list, $ss) = @_;

    my $superscript = $self->{superscript};
    my $line = "";
    my $sep = "";
    foreach my $e (@$list) {
        $line .= $sep;
        $sep = "  ";

        # Reference keys (to be referenced by re_restr, stagk, stagr).
        if (exists $e->[3]) {
            $line .= $$superscript[$ss];
            $e->[3] = $ss++;
        } else {
            $line .= $item;
        }

        # Writing or reading (keb, reb).
        $line .= $e->[0];

        # References to writings (re_restr).
        if (defined $e->[1]) {
            my @s = sort { $a <=> $b } map { $$_ } @{$e->[1]};
            $line .= LOCALCOLOR(color("writing"), @$superscript[@s]);
        }

        # Attributes (ke_inf, ke_pri, re_nokanji, re_inf, re_pri).
        if (defined $e->[2]) {
            my $meta = $self->{meta};
            my @mr = unpack "U*", $e->[2];
            my @ab = map { $meta->[$_][1] } @mr;
            $line .= LOCALCOLOR(color("tag"), join($item, "", @ab));
        }
    }
    print $self->wrap_line($line, 2);

    return $ss;
}


sub print_sense {
    my $self = shift;
    my ($index, $sense, $ss) = @_;

    my $meta = $self->{meta};
    my $superscript = $self->{superscript};

    # Part-of-speech (pos).
    my @mr = unpack "U*", $sense->[2] if defined $sense->[2];
    my @ab;
    for (my $i = 0; $i < @mr; ++$i) {
        my $ki = $meta->[$mr[$i]][0];
        # Comment in JMdict.gz for "misc" says
        #
        #   As with part-of-speech, information will usually apply to
        #   several senses.
        #
        # but this doesn't seem to be the case, so we uplift only "pos".
        if ($ki ne "pos") {
            @ab = map { $meta->[$_][1] } splice(@mr, $i);
            last;
        }
    }
    if (@mr) {
        my @ab = map { $meta->[$_][1] } @mr;
        my $l = LOCALCOLOR(color("tag"), join("; ", @ab));
        print(LOCALCOLOR(color("reset"), "\n"), $self->wrap_line($l, 0),
              LOCALCOLOR(color("reset"), "\n"));
    } elsif ($index == 1) {
        print LOCALCOLOR(color("reset"), "\n");
    }

    # Sense number.
    my $line = sprintf "%3d", $index;

    # References to writings and readings (stagk, stagr).
    if (defined $sense->[1]) {
        my @sw = sort { $a <=> $b } map { $$_ } @{$sense->[1]};
        my @sr;
        for (my $i = 0; $i < @sw; ++$i) {
            if ($sw[$i] >= $ss) {
                @sr = splice(@sw, $i);
                last;
            }
        }
        $line .= LOCALCOLOR(color("writing"), @$superscript[@sw])
            if @sw;
        $line .= LOCALCOLOR(color("reading"), @$superscript[@sr])
            if @sr;
    }

    # Attributes (misc, dial, field).
    $line .= LOCALCOLOR(color("tag"), " ", join(", ", @ab))
        if @ab;

    # Sense info (s_inf).
    $line .= LOCALCOLOR(color("tag"),
                        " (", join("; ", map { $_->[0] } @{$sense->[5]}), ")")
        if defined $sense->[5];

    # Source language (lsource).
    if (defined $sense->[6]) {
        my @sources;
        foreach my $e (@{$sense->[6]}) {
            my $source = " ";
            my $lang = "English";
            if (defined $e->[2]) {
                foreach my $mr (unpack "U*", $e->[2]) {
                    if ($meta->[$mr][0] eq '@xml:lang') {
                        $lang = $meta->[$mr][2];
                        $lang =~ s/\W.*//;
                        last;
                    }
                }
            }
            $source .= $lang;
            $source .= ' "' . $e->[0] . '"' if $e->[0] ne "";
            push @sources, $source;
        }
        $line .= LOCALCOLOR(color("tag"), " [from", join(",", @sources), "]");
    }

    # Separator.
    $line .= ":";

    # Glosses (gloss).
    $line .= " $item" . ($sense->[0] =~ s/\t/  $item/gr)
        if $sense->[0] ne "";

    # Antonym (ant).
    $line .= format_xref("  Antonym: ", $sense->[4])
        if defined $sense->[4];

    # Cross-reference (xref).
    $line .= format_xref("  Also see: ", $sense->[3])
        if defined $sense->[3];

    print $self->wrap_line($line, 5);
}


sub format_xref {
    my ($label, $list) = @_;

    my $text = PUSHCOLOR(color("tag")) . $label;
    my @refs;
    foreach my $e (@$list) {
        my ($w, $r, $i) = split /\./, $e->[0], 3;
        ($r, $i) = (undef, $r) if !defined $i && defined $r && $r =~ /^\d+$/;
        my @p = (LOCALCOLOR(color("writing"), $w));
        push @p, LOCALCOLOR(color("reading"), $r) if defined $r;
        push @p, LOCALCOLOR(color("text"), $i) if defined $i;
        push @refs, join($item, @p);
    }
    $text .= join(", ", @refs) . POPCOLOR;

    return $text;
}


sub wrap_line {
    my $self = shift;
    my ($line, $indent) = @_;

    my $width = $self->{width};
    my $i;
    my $w = 0;
    my $color = TOPCOLOR;
    # For us it's better to produce a short line rather than to
    # overflow, so we treat "East_Asian_Width: Ambiguous" characters
    # as wide, and process each code point in grapheme clusters
    # separately.
    #
    # Regexp below assumes that single color overrides previous color
    # setting.  This is not true in general for inverse, underline,
    # blink, etc., but we do not use those.  To fix this we'd have to
    # concatenate all colors seen to a point, which is an overkill in
    # our case.
    $line =~ s{
        ((?>
            # Eat whitespace without backtracking, accumulate width.
            (?>
                (?:
                    # Colors have zero width and may be part of whitespace.
                    ( \e \[ [^m]* m )
                  | (?= \s ) [\p{Ea=W}\p{Ea=F}\p{Ea=A}] (?{ $w += 2 })
                  | \s                                  (?{ $w += 1 })
                )*
            )
            # Eat a word without backtracking, accumulate width.
            (?>
                (?:
                    # Colors have zero width and may be part of a word.
                    ( \e \[ [^m]* m )
                  | (?= \S ) [\p{Ea=W}\p{Ea=F}\p{Ea=A}] (?{ $w += 2 })
                  | \S                                  (?{ $w += 1 })
                )+
            )
            # If exceeded screen width backtrack to previous <[space,] word>.
            (?(?{ $w <= $width })
                # Success - eat nothing.
              | (*FAIL)  # Failure - backtrack.
            )
        )+)
        # Eat leading whitespace for the next iteration.
        (?: \s | ( \e \[ [^m]* m ) )*
    }{
        my $s = $1;
        if (defined $i) {
            $s = color("reset") . "\n" . $i . $color . $s;
        } else {
            $i = " " x $indent;
        }
        # Apply last seen color to the next iteration.
        $color = $4 // $3 // $2 // TOPCOLOR;
        $w = $indent;
        $s;
    }xeg;

    return $line;
}


sub print_data_info {
    my $self = shift;

    my $dbh = $self->{dbh};

    print @{$dbh->selectcol_arrayref(q{
        SELECT printf('%-10s %s %s'||x'0a', ki, ky, vl)
        FROM kanji_meta
    })};
    print @{$dbh->selectcol_arrayref(q{
        SELECT printf('%-10s %s %s'||x'0a', ki, ky, vl)
        FROM writing_meta
    })};
    print $dbh->selectrow_array(q{
        SELECT printf('%-10s %s %s'||x'0a', ki, ab, ds)
        FROM word_meta
        WHERE id = 0
    });
    print "Total ", $dbh->selectrow_array(q{
        SELECT max(id)
        FROM word
    }), " words, ";
    print $dbh->selectrow_array(q{
        SELECT count(DISTINCT kc)
        FROM kanji
    }), " kanji (";
    print $dbh->selectrow_array(q{
        SELECT count(DISTINCT kc)
        FROM cangjie
    }), " with Cangjie code), ";
    print $dbh->selectrow_array(q{
        SELECT count(DISTINCT cc)
        FROM writing
    }), " handwritings.\n";
    print "Kanji without Cangjie code:\n";
    print $dbh->selectrow_array(q{
        SELECT group_concat(char(kc), '')
        FROM (SELECT DISTINCT kc
              FROM kanji
              WHERE NOT EXISTS (SELECT * FROM cangjie WHERE kc = kanji.kc)
              ORDER BY kc)
    }), "\n";
}


BEGIN {
    # Data from http://www.unicode.org/Public/8.0.0/ucd/CJKRadicals.txt .
    # Note that radicals are from CJK unified ideographs, not U+2f00-1+RN.
    @radical = (undef, qw(
        一     丨     丶     丿     乙     亅     二     亠     人     儿
        入     八     冂     冖     冫     几     凵     刀     力     勹
        匕     匚     匸     十     卜     卩     厂     厶     又     口
        囗     土     士     夂     夊     夕     大     女     子     宀
        寸     小     尢     尸     屮     山     巛     工     己     巾
        干     幺     广     廴     廾     弋     弓     彐     彡     彳
        心     戈     戶     手     支     攴     文     斗     斤     方
        无     日     曰     月     木     欠     止     歹     殳     毋
        比     毛     氏     气     水     火     爪     父     爻     爿(丬)
        片     牙     牛     犬     玄     玉     瓜     瓦     甘     生
        用     田     疋     疒     癶     白     皮     皿     目     矛
        矢     石     示     禸     禾     穴     立     竹     米     糸(纟)
        缶     网     羊     羽     老     而     耒     耳     聿     肉
        臣     自     至     臼     舌     舛     舟     艮     色     艸
        虍     虫     血     行     衣     襾     見(见) 角     言(讠) 谷
        豆     豕     豸     貝(贝) 赤     走     足     身     車(车) 辛
        辰     辵(辶) 邑     酉     釆     里     金(钅) 長(长) 門(门) 阜
        隶     隹     雨     靑     非     面     革     韋(韦) 韭     音
        頁(页) 風(风) 飛(飞) 食(饣) 首     香     馬(马) 骨     高     髟
        鬥     鬯     鬲     鬼     魚(鱼) 鳥(鸟) 鹵(卤) 鹿     麥(麦) 麻
        黃(黄) 黍     黑     黹     黽(黾) 鼎     鼓     鼠     鼻     齊(齐)
        齒(齿) 龍(龙) 龜(龟) 龠
    ));

    # Cangjie help is based on https://zh.wikibooks.org/wiki/%E5%80%89%E9%A0%A1%E8%BC%B8%E5%85%A5%E6%B3%95/%E5%80%89%E9%A0%A1%E5%AD%97%E6%AF%8D%E8%88%87%E8%BC%94%E5%8A%A9%E5%AD%97%E5%BD%A2#.E8.BC.94.E5.8A.A9.E5.AD.97.E5.BD.A2 .
    $cangjie_help = <<END;
A 日曰 t巴眉                    M 一㇀厂⼯ t百石豕 bl光壯
B 月冂冖爫爫 t炙                N 弓亅㇖㇇㇕⺈㇠㇈㇅⺄ o夕 b今 tr阝乃朵設
C 金丷 t分 b只 r扒 o小 i四      O 人亻 t乞気 l丘邱 r豕乑飞兆 br尺夫
D 木 b子 r寸也 t夬皮韋 i五      P 心忄匕七勹 b恭 a比 l切世 r代民式曳 o勿
E 水氺氵 a双 b支 br及           Q 手扌 t青 b羊年韋 l夫那 tr桀 i着
F 火⺌⺍灬 t尙 i平 b不礻示      R 口
G 土 l垃                        S 尸㇆匚 t己 o⺕尹 a巨 l耳 b乍
H 竹⺮㇒⺁ t千白臼𦥑 l牛后凡    T 廿卝廾艹艹艹䒑 i庶曲虛 t革昔共 o卅 b皿豆立關
I 戈丶广厶 t台 b云去 l刃        U 山凵⺃屮屮 br先氾 r孔 bl朔
J 十宀 i毋 l戎                  V 女㇛㇜㇙㇂ t幺 a巛 l收以 b亡甚艮展衣鼠
K 大乂疒 l右力九 b耂文父丈 t⺨  W 田 o図圕罒母毋
L 中丨衤⺺ a川 t隶              Y 卜亠⺀辶辶 t上貞文主言幷 b下冬寒 r朴外 i母図
    t=top    b=bottom    l=left    r=right    o=outer    i=inner    a=all
END
}


1
