# -*- encoding: utf-8 -*-
#
# Copyright (C) 2016-2017 Tomash Brechko.  All rights reserved.
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
package GeekJDict::Update::Words;
use parent "GeekJDict::Update";

use strict;
use warnings qw(all);
use utf8;


use GeekJDict::Util qw(get_terms);


my $wadoku_tab = "WaDokuNormal.tab";


sub new {
    my $class = shift;
    my ($option, $jmdict, $wadoku) = @_;

    my $self = $class->SUPER::new($option, $jmdict, expand_entities => 0);

    @{$self->{lang}}{ @{$option->{lang}} } = () if $option->{lang};

    $self->{word_id} = 1000000;  # Leave room for renumbering.

    if ($wadoku) {
        use Archive::Zip;
        use Archive::Zip::MemberRead;
        Archive::Zip::setErrorHandler(sub {
            # Error reporting in Archive::Zip doesn't always give file name.
            my $message = join "", @_;
            $message = "$wadoku: $message" if index($message, $wadoku) == -1;
            die $message;
        });
        my $zip = Archive::Zip->new($wadoku);
        my $wadoku_tab = $zip->memberNamed($wadoku_tab)
            or die "Can't find $wadoku_tab inside $wadoku\n";

        $self->{wadoku_tab} = $wadoku_tab->readFileHandle();
    }

    return $self;
}


sub _create_tables {
    my $self = shift;

    $self->set_db_version("words");

    my $dbh = $self->{dbh};

    # word_meta table:
    #  id - row ID.
    #  ki - record kind.
    #  ab - abbreviation.
    #  ds - description.
    $dbh->do(q{ DROP TABLE IF EXISTS word_meta });
    $dbh->do(q{
        CREATE TABLE word_meta (
            id INTEGER PRIMARY KEY,
            ki TEXT NOT NULL,
            ab TEXT NOT NULL,
            ds TEXT NOT NULL
        )
    });
    $self->{meta_insert} = $dbh->prepare(q{
        INSERT INTO word_meta (id, ki, ab, ds)
        VALUES (?, ?, ?, ?)
    });

    # word table:
    #  id - word ID.
    #  it - index [*:3], type [2:0].  Type is:
    #         i 0 - keb
    #         i 1 - reb
    #         i 2 - sense (tx is tab-separated glosses)
    #         i 3 - xref
    #         i 4 - ant
    #         i 5 - s_inf
    #         i 6 - lsource
    #         i 7 - pronunciation info (accent, devoiced, non-fricative, etc.)
    #  tx - keb/reb/xref/ant/s_inf/lsource/glosses text, or pronunciation
    #       info as a sequence of Unicode character codes each encoding
    #       offset[*:2], type[1:0], where type is:
    #         i 0 - accent downstep (between morae)
    #         i 1 - devoiced i or u sound
    #         i 2 - non-fricative g sound
    #         i 3 - は or へ particle read as わ or え respectively
    #  jr - re_restr/stagk/stagr/sense as a list of references to it (each
    #       Unicode character code is a reference to it, id is implicit).
    #  mr - metadata as a list of references to word_meta.id (each
    #       Unicode character code is a reference to word_meta.id).
    $dbh->do(q{ DROP TABLE IF EXISTS word });
    $dbh->do(q{
        CREATE TABLE word (
            id INTEGER NOT NULL,
            it INTEGER NOT NULL,
            tx TEXT NOT NULL,
            jr TEXT,
            mr TEXT,

            PRIMARY KEY (id, it)
        ) WITHOUT ROWID
    });
    $self->{word_insert} = $dbh->prepare(q{
        INSERT INTO word (id, it, tx, jr, mr)
        VALUES (?, ?, ?, ?, ?)
    });
}


my %reference;


sub _init_parser {
    my $self = shift;

    my $xml = $self->{xml};

    use XML::LibXML::Reader qw(:types);

    unless ($xml->read && $xml->nodeType == XML_READER_TYPE_COMMENT
            && $xml->value =~ /^\s*\QRev 1.08\E\b/) {
        my $revision = $xml->value =~ s/\n.*//mr;
        die "unsupported JMdict DTD $revision, expected 1.08\n";
    }
    while ($xml->read && $xml->nodeType != XML_READER_TYPE_ELEMENT) {
        if ($xml->nodeType == XML_READER_TYPE_COMMENT
            && $xml->value =~ /^\s*JMdict created:\s*(.+?)\s*$/) {
            $self->{meta_insert}->execute(0, "JMdict", "created", $1);
        }
    }

    my (%word, $group);
    my $append_group = sub {
        $group = {};
        push @{$word{$_[0]}}, $group;
    };
    my $append_xml = sub {
        push @{$group->{$_[0]}}, $xml->readInnerXml;
    };
    my $set_xml = sub {
        $group->{$_[0]} = $xml->readInnerXml;
    };
    my $append_text = sub {
        my $text = $xml->copyCurrentNode(1)->textContent;
        $text =~ tr/\t/ /;
        push @{$group->{$_[0]}}, $text;
    };
    my $append_lsource = sub {
        my $text = $xml->copyCurrentNode(1)->textContent;
        $text =~ tr/\t/ /;
        push @{$group->{$_[0]}}, $text, $xml->xmlLang // "eng";
    };
    my $append_reference = sub {
        $xml->read;
        my $name = $xml->name;
        my $ref = \$reference{"$_[0]/$name"};
        $$ref = $xml->copyCurrentNode(0)->textContent unless $$ref;
        push @{$group->{$_[0]}}, $name;
    };
    my $append_nokanji = sub {
        push @{$group->{$_[0]}}, "NoK";
    };
    my $append_priority = sub {
        my $pri = $xml->readInnerXml;
        # news1/2 are redundant: news1 = nf01..nf24, news2 = nf25..nf48
        push @{$group->{$_[0]}}, $pri unless $pri =~ /^news[12]$/;
    };

    my %parser = (
        ent_seq      => sub { $self->_process_word(\%word) if %word;
                              %word = ();
                              $group = \%word; },
         keb         => sub { $append_group->("k_ele"); $set_xml->(@_); },
          ke_inf     => $append_reference,
          ke_pri     => $append_priority,
         reb         => sub { $append_group->("r_ele"); $set_xml->(@_); },
          re_nokanji => $append_nokanji,
          re_restr   => $append_xml,
          re_inf     => $append_reference,
          re_pri     => $append_priority,
         sense       => $append_group,
          stagk      => $append_xml,
          stagr      => $append_xml,
          pos        => $append_reference,
          xref       => $append_xml,
          ant        => $append_xml,
          field      => $append_reference,
          misc       => $append_reference,
          s_inf      => $append_text,
          lsource    => $append_lsource,
          dial       => $append_reference,
          gloss      => do {
              if ($self->{lang}) {
                  sub { my $lang = $xml->xmlLang // "eng";
                        $append_text->(@_)
                            if exists $self->{lang}->{$lang}; };
              } else {
                  $append_text;
              }
          },
        _END         => sub { $self->_process_word(\%word) if %word; }
    );

    return \%parser;
}


sub _encode_mr {
    my $self = shift;

    pack "U*", map {
        my $key = "$_[0]/$_";
        my $ref = \$self->{metaref}{$key};
        unless ($$ref) {
            $$ref = keys %{$self->{metaref}};
            $self->{meta_insert}->execute($$ref, $_[0], $_, $reference{$key});
        }
        $$ref;
    } @{$_[1]};
}


sub _encode_jr {
    pack "U*", @{$_[1]}{@{$_[0]}};
}


sub _process_word {
    my $self = shift;
    my ($word) = @_;

    my $id = ++$self->{word_id};

    my $keb_it = 0;
    my $reb_it = 1;
    my $sense_it = 2;
    my $xref_it = 3;
    my $ant_it = 4;
    my $s_inf_it = 5;
    my $lsource_it = 6;

    my (%kit, %rit);
    if (exists $word->{k_ele}) {
        foreach my $k_ele (@{$word->{k_ele}}) {
            my $tx = $k_ele->{keb};
            $kit{$tx} = $keb_it;

            my $mr = "";
            $mr .= $self->_encode_mr(pri => $k_ele->{ke_pri})
                if exists $k_ele->{ke_pri};
            $mr .= $self->_encode_mr(ke_inf => $k_ele->{ke_inf})
                if exists $k_ele->{ke_inf};
            $mr = undef if $mr eq "";

            $self->{word_insert}->execute($id, $keb_it, $tx, undef, $mr);
            $keb_it += 8;
        }
    }

    foreach my $r_ele (@{$word->{r_ele}}) {
        my $tx = $r_ele->{reb};
        $rit{$tx} = $reb_it;

        my $jr;
        if (exists $r_ele->{re_restr}) {
            $jr = _encode_jr($r_ele->{re_restr} => \%kit);
        }

        my $mr = "";
        $mr .= $self->_encode_mr(pri => $r_ele->{re_pri})
            if exists $r_ele->{re_pri};
        $mr .= $self->_encode_mr(re_inf => $r_ele->{re_inf})
            if exists $r_ele->{re_inf};
        $mr .= $self->_encode_mr(nokanji => $r_ele->{re_nokanji})
            if exists $r_ele->{re_nokanji};
        $mr = undef if $mr eq "";

        $self->{word_insert}->execute($id, $reb_it, $tx, $jr, $mr);
        $reb_it += 8;
    }

    foreach my $sense (@{$word->{sense}}) {
        my $tx = exists $sense->{gloss} ? join("\t", @{$sense->{gloss}}) : "";

        my $jr = "";
        $jr .= _encode_jr($sense->{stagk} => \%kit)
            if exists $sense->{stagk};
        $jr .= _encode_jr($sense->{stagr} => \%rit)
            if exists $sense->{stagr};
        $jr = undef if $jr eq "";

        my $mr = "";
        $mr .= $self->_encode_mr(pos => $sense->{pos})
            if exists $sense->{pos};
        $mr .= $self->_encode_mr(misc => $sense->{misc})
            if exists $sense->{misc};
        $mr .= $self->_encode_mr(dial => $sense->{dial})
            if exists $sense->{dial};
        $mr .= $self->_encode_mr(field => $sense->{field})
            if exists $sense->{field};
        $mr = undef if $mr eq "";

        $self->{word_insert}->execute($id, $sense_it, $tx, $jr, $mr);
        $jr = pack "U", $sense_it;
        $sense_it += 8;

        if (exists $sense->{xref}) {
            foreach my $tx (@{$sense->{xref}}) {
                # Substitute CJK center-dot with ASCII dot.
                $tx =~ s/・/./g;
                $self->{word_insert}->execute($id, $xref_it, $tx, $jr, undef);
                $xref_it += 8;
            }
        }

        if (exists $sense->{ant}) {
            foreach my $tx (@{$sense->{ant}}) {
                # Substitute CJK center-dot with ASCII dot.
                $tx =~ s/・/./g;
                $self->{word_insert}->execute($id, $ant_it, $tx, $jr, undef);
                $ant_it += 8;
            }
        }

        if (exists $sense->{s_inf}) {
            foreach my $tx (@{$sense->{s_inf}}) {
                $self->{word_insert}->execute($id, $s_inf_it, $tx, $jr, undef);
                $s_inf_it += 8;
            }
        }

        if (exists $sense->{lsource}) {
            while (my ($tx, $lang) = splice(@{$sense->{lsource}}, 0, 2)) {
                my $mr;
                if ($lang ne "eng") {
                    my $kind = '@xml:lang';
                    my $ref = \$reference{"$kind/$lang"};
                    use Locale::Codes::Language;
                    $$ref = code2language($lang, "alpha-3") unless $$ref;
                    $mr = $self->_encode_mr($kind => [$lang]);
                }

                $self->{word_insert}->execute($id, $lsource_it, $tx, $jr, $mr);
                $lsource_it += 8;
            }
        }
    }
}


sub _finalize {
    my $self = shift;

    $self->_reorder;
    $self->_create_index;

    $self->_annotate_pronunciation if exists $self->{wadoku_tab};

    $self->_limit_cangjie
        if $self->{dbh}->tables(undef, undef, "cangjie", "TABLE");
}


my @static_order = qw(
    pri/nf01 pri/nf02 pri/nf03 pri/nf04 pri/nf05 pri/nf06 pri/nf07 pri/nf08
    pri/nf09 pri/nf10 pri/nf11 pri/nf12 pri/nf13 pri/nf14 pri/nf15 pri/nf16
    pri/ichi1
    pri/spec1
    pri/nf17 pri/nf18 pri/nf19 pri/nf20 pri/nf21 pri/nf22 pri/nf23 pri/nf24
    pri/nf25 pri/nf26 pri/nf27 pri/nf28 pri/nf29 pri/nf30 pri/nf31 pri/nf32
    pri/ichi2
    pri/spec2
    pri/nf33 pri/nf34 pri/nf35 pri/nf36 pri/nf37 pri/nf38 pri/nf39 pri/nf40
    pri/nf41 pri/nf42 pri/nf43 pri/nf44 pri/nf45 pri/nf46 pri/nf47 pri/nf48
    pri/gai1
    pri/gai2
    ...
    field/*
    misc/sens
    misc/derog
    misc/vulg
    misc/rare
    misc/obs
    misc/arch
    pos/adj-shiku
    pos/adj-ku
    pos/v[24]*
);


package GeekJDict::Update::Words::StaticRank;
our (%renumber, $middle);
sub new { bless { $middle => undef }, $_[0] }
sub step {
    return unless defined $_[1];
    utf8::decode($_[1]);
    my @keys = grep { exists $renumber{$_} } unpack "U*", $_[1];
    @{$_[0]}{ map { $renumber{$_} } @keys } = ();
}
# We have to prefix packed string with a letter so that it won't look
# like a number, otherwise DBD::SQLite (1.48) will coerce it possibly
# stripping whitespace which is significant for us.
sub finalize { "R" . pack "U*", sort { $a <=> $b } keys %{$_[0]} }
package GeekJDict::Update::Words;


sub _reorder {
    my $self = shift;

    my $dbh = $self->{dbh};

    my $meta_id = $dbh->prepare(q{
        SELECT id
        FROM word_meta
        WHERE ki GLOB ? AND ab GLOB ?
    });
    %GeekJDict::Update::Words::StaticRank::renumber = ();
    while (my ($i, $o) = each @static_order) {
        if ($o ne "...") {
            my ($ki, $ab) = split "/", $o, 2;
            my $ids = $dbh->selectcol_arrayref($meta_id, undef, $ki, $ab);
            @GeekJDict::Update::Words::StaticRank::renumber{@$ids} =
                ($i) x @$ids;
        } else {
            $GeekJDict::Update::Words::StaticRank::middle = $i;
        }
    }
    use DBD::SQLite;
    $self->{dbh}->sqlite_create_aggregate(
        "static_rank", 1, "GeekJDict::Update::Words::StaticRank",
        DBD::SQLite::Constants::SQLITE_DETERMINISTIC);
    my $order = $dbh->selectcol_arrayref(q{
        SELECT id
        FROM word
        GROUP BY id
        ORDER BY static_rank(mr), id
    });
    my $renumber = $dbh->prepare(q{
        UPDATE word
        SET id = ?+1
        WHERE id = ?
    });
    while (my @r = each @$order) {
        $renumber->execute(@r);
    }
}


sub _create_index {
    my $self = shift;

    my $dbh = $self->{dbh};

    my %terms;
    my $select = $dbh->prepare(q{
        SELECT id, group_concat(tx, ' ')
        FROM word
        WHERE it & 7 <= 2
        GROUP BY id
        ORDER BY id
    });
    $select->bind_columns(\my ($id, $tx));
    $select->execute;
    while ($select->fetch) {
        my @terms = get_terms($tx);
        foreach my $t (@terms) {
            push @{$terms{$t}}, $id;
        }
    }

    # term_index table:
    #  tx - term text
    #  wi - difference list of word.id encoded with Perl's pack "w*"
    $dbh->do(q{ DROP TABLE IF EXISTS term_index });
    $dbh->do(q{
        CREATE TABLE term_index (
            tx TEXT NOT NULL PRIMARY KEY,
            wi BLOB NOT NULL
        ) WITHOUT ROWID
    });
    my $insert = $dbh->prepare(q{
        INSERT INTO term_index (tx, wi)
        VALUES (?, ?)
    });

    while (my ($tx, $id) = each %terms) {
        for (my $i = @$id - 1; $i > 0; --$i) {
            $id->[$i] -= $id->[$i - 1];
        }
        my $wi = pack "w*", @$id;
        $insert->bind_param(1, $tx, DBI::SQL_VARCHAR);
        $insert->bind_param(2, $wi, DBI::SQL_BLOB);
        $insert->execute;
    }
}


sub _annotate_pronunciation {
    my $self = shift;

    my $dbh = $self->{dbh};

    my $select_wi = $dbh->prepare(q{
        SELECT wi
        FROM term_index
        WHERE tx = ?
    });
    my $select_tx = $dbh->prepare(q{
        SELECT it, tx
        FROM word
        WHERE id = ? AND it & 7 < 2
    });

    # Parsing of WaDokuNormal.tab is somewhat relaxed.
    my %info;
    $self->{wadoku_tab}->input_record_separator("\r");
    while (my $line = $self->{wadoku_tab}->getline) {
        utf8::decode($line);
        my ($w, $a, $a2, $m) = (split /\t/, $line)[1,6,7,13];

        $a = undef unless $a =~ /^\s*\d+\s*$/;
        $a = $a2 if !defined $a && $a2 =~ /^\s*\d+\s*(?:$|;|,)/;
        $m = join "", grep { defined } $m =~ m{
            ( [\p{Hiragana}ー]+ )
          | ( \[Dev\] ) \P{Hiragana}* (っ?) \P{Hiragana}* ([きしちひぴくすつふぷ])
          | ( \[NN\] )  \P{Hiragana}* (っ?) \P{Hiragana}* ([がぎぐげご])
          | ( \[Jo\] )  \P{Hiragana}* ([はへ])
        }xg;
        my @r = $m =~ /(\p{Hiragana}[ゃゅょ]?|ー)/g;
        next unless (defined $a && $a <= @r) || $m =~ /\[/;

        my $p = "";
        # Accent MUST come first (see CLI.pm:print_japanese()).
        if (defined $a && $a <= @r) {
            $a = length join "", @r[0..$a-1] if $a > 0;
            $p = pack "U", $a << 2 | 0;
        }
        if ($m =~ /\[/) {
            my $o = 0;
            foreach my $m (split /\[([^]]+)\]/, $m) {
                if ($m eq "Dev") {
                    $p .= pack "U", $o << 2 | 1;
                } elsif ($m eq "NN") {
                    $p .= pack "U", $o << 2 | 2;
                } elsif ($m eq "Jo") {
                    $p .= pack "U", $o << 2 | 3;
                } else {
                    $o += length $m;
                }
            }
        }

        $w =~ s/･//g;
        $w =~ s/(.)々/$1$1/g;
        my %w;
        @w{grep { /\p{Han}/ }
           $w =~ /([\p{Han}\p{Block: Hiragana}\p{Block: Katakana}]+)/g} = ();
        my $r = join "", @r;

        my $wi = $dbh->selectrow_array($select_wi, undef, $r);
        unless (defined $wi) {
            next unless $w =~ /\p{Katakana}/;
            $r =~ tr/\x{3041}-\x{3096}/\x{30a1}-\x{30f6}/;
            $wi = $dbh->selectrow_array($select_wi, undef, $r);
            next unless defined $wi;
        }
        my $id = 0;
        foreach my $d (unpack "w*", $wi) {
            $id += $d;
            $select_tx->bind_columns(\my ($it, $tx));
            $select_tx->execute($id);
            my ($w, $rit);
            while ($select_tx->fetch) {
                if (($it & 7) == 0) {
                    $w = exists $w{$tx} unless $w;
                } elsif ($tx eq $r) {
                    $rit = $it;
                    last if $w;
                }
            }
            if (defined $rit && ($w || (!defined $w && !keys %w))) {
                unless (exists $info{"$id $rit"}) {
                    $info{"$id $rit"} = $p;
                } elsif ($info{"$id $rit"} ne $p) {
                    $info{"$id $rit"} = "";
                }
            }
        }
    }

    my $insert = $dbh->prepare(q{
        INSERT INTO word (id, it, tx)
        VALUES (?+0, ?|7, ?)
    });
    while (my ($k, $v) = each %info) {
        next if $v eq "";
        my ($id, $it) = split / /, $k;
        $insert->execute($id, $it, $v);
    }
}


BEGIN {
    %reference = (
        'pri/ichi1' => "common word from \"Ichimango goi bunruishuu\","
                     . " Senmon Kyouiku Publishing, Tokyo, 1998",
        'pri/ichi2' => "less common word from \"Ichimango goi bunruishuu\","
                     . " Senmon Kyouiku Publishing, Tokyo, 1998",
        'pri/spec1' => "common word",
        'pri/spec2' => "less common word",
        'pri/gai1' => "common loanword from \"wordfreq\" file",
        'pri/gai2' => "less common loanword from \"wordfreq\" file",

        # Convert re_nokanji to this one.
        'nokanji/NoK' => "not a true reading of kanji",
    );
    for my $i (1..48) {
        my $to = $i * 500;
        my $from = $to - 499;
        $from =~ s/(.)(...)$/$1,$2/;
        $to =~ s/(.)(...)$/$1,$2/;
        $reference{sprintf "pri/nf%02d", $i} =
            "falls within $from--$to of 24,000 words from \"wordfreq\" file";
    }
}


1
