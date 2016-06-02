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
package GeekJDict::ReadLine;

use strict;
use warnings qw(all);
use utf8;

use Term::ReadLine;
use GeekJDict::Util qw(globs2regexps);
use GeekJDict::Romaji qw(jconvert jconvert_last);


sub new {
    my $class = shift;
    my ($kana_inhibitor, $full_cangjie,
        $complete_kanji, $display_kanji_list, $cangjie_help) = @_;

    my $readline = Term::ReadLine->new("GeekJDict", \*STDIN, \*STDOUT);
    $readline->ornaments(0);
    $readline->MinLine(undef);  # We will add to history ourselves.

    my $attribs = $readline->Attribs;

    # Readline handles SIGWINCH only while it is running.  This is not
    # enough as the size can change in other moments.  The hook below
    # will be called after signal handlers are installed, so there
    # will be no race window.
    $attribs->{pre_input_hook} = sub { $readline->reset_screen_size };

    # Setup Japanese input modes.
    my ($prompt, $map) = ("geekjdict>> ", 0);

    my ($point, $line, $replacement);
    my $get_line = sub {
        $point = $attribs->{point};
        $line = substr($attribs->{line_buffer}, 0, $point);
        utf8::decode($line);
    };
    my $update_line = sub {
        my $bytes = utf8::upgrade($line);  # Clobbers $line.
        my $new_point = $point - $bytes;
        # At least in Readline 6.3 mark handling is broken: its
        # relative position is not preserved (as in Emacs) and it may
        # end up in a middle of a multibyte character in many cases.
        # So let's at least move it to a safer place.
        $attribs->{point} = $attribs->{mark} = $new_point;

        $readline->begin_undo_group;
        $readline->delete_text($new_point, $point);
        $readline->insert_text($replacement);
        $readline->end_undo_group;
    };

    # geekjdict-input-mode function switches input mode to hiragana
    # (M-h), katakana (M-k), or global (M-g).  Additionally uppercase
    # versions (i.e. M-H, M-K, M-G) convert last N words (N is
    # universal argument) according to new input mode (conversion is
    # lossless, so it may be used in cases when you typed words in
    # wrong mode).
    my $mode = sub {
        my ($count, $key) = @_;

        my $char = chr $key;
        my $lchar = lc $char;
        # We could use ひらがな and カタカナ here, but this would make
        # prompt visually indecipherable from the output.
        if ($lchar eq "h") {
            $prompt = "hiragana>>> ";
            $map = 1;
        } elsif ($lchar eq "k") {
            $prompt = "katakana>>> ";
            $map = 2;
        } else {
            $prompt = "geekjdict>> ";
            $map = 0;
        }
        $readline->set_prompt($prompt);

        if ($lchar ne $char && $count > 0) {
            $get_line->();

            $replacement = reverse $line;
            $line = "";
            # Readline uses iswalnum() for word characters, but we
            # include dash and apostrophe because in kana they are
            # parts of a word.
            while ($count-- > 0
                   && $replacement =~ /\G([^[:alnum:]'-]*[[:alnum:]'-]+)/g) {
                $line .= $1;
            }
            $replacement = reverse $line;
            $replacement = jconvert(0, $replacement);
            $replacement = jconvert($map, $replacement) if $map;

            $update_line->();
        }

        return 0;
    };
    my $mode_func = $readline->add_defun("geekjdict-input-mode", $mode);
    foreach my $k (qw(h H k K g G)) {
        $readline->bind_keyseq("\e$k", $mode_func);
    }

    # geekjdict-kana-insert command implements hiragana/katakana input.
    my $kana = sub {
        my ($count, $key) = @_;

        return 0 if $count <= 0;

        my $char = chr $key;
        unless ($map) {
            $readline->insert_text($char x $count);
        } else {
            $get_line->();

            if ($line =~ $kana_inhibitor) {
                $readline->insert_text($char x $count);
            } else {
                substr($line, 0, -3, "");
                $replacement = jconvert_last($map, $line . $char);

                $update_line->();
            }
        }

        return 0;
    };
    my $kana_func = $readline->add_defun("geekjdict-kana-insert", $kana);
    foreach my $k (qw(a i u e o  n -)) {
        $readline->bind_key(ord $k, $kana_func);
    }

    # Kanji input completion.
    my ($limit_cangjie, $completions);
    # Do not ask for conformation if too many possible completions.
    $attribs->{completion_query_items} = -1;
    # Completions are already sorted...
    $attribs->{sort_completion_matches} = 0;
    # ...and there are no duplicates.
    $attribs->{ignore_completion_duplicates} = 0;
    # Enforce show-all-if-unmodified regardless of user settings.
    $readline->variable_bind("show-all-if-ambiguous", "off");
    $readline->variable_bind("show-all-if-unmodified", "on");
    # Kanji completion and display functions.
    $attribs->{attempted_completion_function} = sub {
        # We don't need UTF-8 here, so no utf8::decode().
        $limit_cangjie = substr($_[1], 0, $_[2]) !~ $full_cangjie;
        return undef;
    };
    $attribs->{completion_entry_function} = sub {
        my ($text, $index) = @_;

        unless ($index) {
            utf8::decode($text);
            my $type = chr $attribs->{completion_type};
            $completions = $complete_kanji->($type, $text, $limit_cangjie);
        }

        if ($index < @$completions) {
            return $completions->[$index];
        }
        $completions = undef;

        # Do not append space to completed kanji.
        $attribs->{completion_suppress_append} = 1;

        # Do not add closing quote (doesn't seem to work with readline
        # 6.3 and Term::ReadLine::Gnu 1.26).
        $attribs->{completion_suppress_quote} = 1;

        return undef;
    };
    $attribs->{completion_display_matches_hook} = sub {
        my ($matches, $len, $max) = @_;

        my (undef, $width) = $readline->get_screen_size;
        $display_kanji_list->($matches, $max, $width);

        # This is what readline-6.3 does after displaying completions.
        $readline->forced_update_display;
    };

    my $help = sub {
        print "\n", $cangjie_help;
        $readline->forced_update_display;
    };
    my $help_func = $readline->add_defun("geekjdict-cangjie-help", $help);
    $readline->bind_keyseq("\eC", $help_func);

    return bless({ readline => $readline, prompt => \$prompt, last_cmd => "" },
                 $class);
}


sub readline {
    my $self = shift;

    my $readline = $self->{readline};
    while (defined (my $input = $readline->readline(${$self->{prompt}}))) {
        utf8::decode($input);

        # Ignore empty lines.
        next unless $input =~ /\S/;

        # Strip trailing spaces.
        $input =~ s/\s+$//;

        # Add to history unless line starts with space or equals to last input.
        unless ($input =~ s/^\s+// || $input eq $self->{last_cmd}) {
            $readline->addhistory($input);
            $self->{last_cmd} = $input;
        }

        # Remove trailing comments.
        $input =~ s/\s*#.*//;

        # Skip comment-only lines.
        next if $input eq "";

        return $input;
    }
    return undef;
}


sub get_screen_size {
    my $self = shift;

    return $self->{readline}->get_screen_size;
}


sub print_history {
    my $self = shift;
    my ($input) = @_;

    my @regexps = map { qr/(?:^|\s)$_(?:\s|$)/i } globs2regexps($input);

    my $readline = $self->{readline};
    my $attribs = $readline->Attribs;
    my $base = $attribs->{history_base};
    my $size = $attribs->{history_length};
    my $offset = $base + $size;
    while ($size-- > 0) {
        my $line = $readline->history_get(--$offset);
        utf8::decode($line);
        next if $line =~ /^h(?:\s|$)/;  # Hide "show history" commands.
        next if grep { $line !~ $_ } @regexps;
        print $line, "\n";
    }
}


1
