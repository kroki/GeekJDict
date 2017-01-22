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
package GeekJDict::Update::Kanji;
use parent "GeekJDict::Update";

use v5.16;
use strict;
use warnings qw(all);


my $unihan_readings = "Unihan_Readings.txt";
my $unihan_codes = "Unihan_DictionaryLikeData.txt";
my $unihan_radical = "Unihan_RadicalStrokeCounts.txt";


sub new {
    my $class = shift;
    my ($option, $kanjidic2, $unihan, $cangjie5) = @_;

    open(my $cangjie, "<:utf8", $cangjie5)
        or die "Can't open $cangjie5: $!\n";

    use Archive::Zip;
    use Archive::Zip::MemberRead;
    Archive::Zip::setErrorHandler(sub {
        # Error reporting in Archive::Zip doesn't always give file name.
        my $message = join "", @_;
        $message = "$unihan: $message" if index($message, $unihan) == -1;
        die $message;
    });
    my $zip = Archive::Zip->new($unihan);
    my $unihan_readings = $zip->memberNamed($unihan_readings)
        or die "Can't find $unihan_readings inside $unihan\n";
    my $unihan_codes = $zip->memberNamed($unihan_codes)
        or die "Can't find $unihan_codes inside $unihan\n";
    my $unihan_radical = $zip->memberNamed($unihan_radical)
        or die "Can't find $unihan_radical inside $unihan\n";

    my $self = $class->SUPER::new($option, $kanjidic2);

    $self->{cangjie5} = $cangjie;

    $self->{unihan_readings} = $unihan_readings->readFileHandle();
    $self->{unihan_codes} = $unihan_codes->readFileHandle();
    $self->{unihan_radical} = $unihan_radical->readFileHandle();

    if ($option->{lang}) {
        foreach my $lang (@{$option->{lang}}) {
            use Locale::Codes::Language;
            my $l = language2code(code2language($lang, "alpha-3"), "alpha-2");
            $self->{lang}->{$l} = $lang;
        }
    }

    return $self;
}


sub _create_tables {
    my $self = shift;

    $self->set_db_version("kanji");

    my $dbh = $self->{dbh};

    # kanji_meta table:
    #  id - row ID.
    #  ki - record kind.
    #  ky - record key.
    #  vl - record value.
    $dbh->do(q{ DROP TABLE IF EXISTS kanji_meta });
    $dbh->do(q{
        CREATE TABLE kanji_meta (
            id INTEGER PRIMARY KEY,
            ki TEXT NOT NULL,
            ky TEXT NOT NULL,
            vl TEXT NOT NULL
        )
    });
    $self->{meta_insert} = $dbh->prepare(q{
        INSERT INTO kanji_meta (ki, ky, vl)
        VALUES (?, ?, ?)
    });

    # kanji table:
    #  kc - kanji code point.
    #  tp - type:
    #         0 - on readings
    #         1 - kun readings
    #         2 - nanoris
    #         3 - meanings
    #         4 - radical
    #         5 - strokes
    #         6 - freq
    #         7 - grade
    #         8 - variants
    #  tx - text of the above (tab-separated for types 0-3, number for
    #  4-7, string of kanji for 8).
    $dbh->do(q{ DROP TABLE IF EXISTS kanji });
    $dbh->do(q{
        CREATE TABLE kanji (
            kc INTEGER NOT NULL,
            tp INTEGER NOT NULL,
            tx NOT NULL,

            PRIMARY KEY (kc, tp)
        ) WITHOUT ROWID
    });
    $self->{kanji_insert} = $dbh->prepare(q{
        INSERT INTO kanji (kc, tp, tx)
        VALUES (?, ?, ?)
    });
    $self->{kanji_insert_int} = $dbh->prepare(q{
        INSERT INTO kanji (kc, tp, tx)
        VALUES (?, ?, ?+0)
    });
    $self->{kanji_append} = $dbh->prepare(q{
        UPDATE kanji
        SET tx = tx||?3
        WHERE kc = ?1 AND tp = ?2
    });

    # cangjie table:
    #  kc - kanji code point.
    #  ti - type[4:3], index[2:0]:
    #         0 i - codes originally without 'X', basic form (A-W, Y) at 0 0
    #         1 i - codes with 'X' removed
    #       bit 4 is set if kanji is not used in any dictionary word
    #  tx - Cangjie code.
    $dbh->do(q{ DROP TABLE IF EXISTS cangjie });
    $dbh->do(q{
        CREATE TABLE cangjie (
            kc INTEGER NOT NULL,
            ti INTEGER NOT NULL,
            tx TEXT NOT NULL,

            PRIMARY KEY (kc, ti)
        ) WITHOUT ROWID
    });
    $self->{cangjie_insert} = $dbh->prepare(q{
        INSERT INTO cangjie (kc, ti, tx)
        VALUES (?, ?, ?)
    });
}


sub _init_parser {
    my $self = shift;

    my $xml = $self->{xml};

    use XML::LibXML::Reader qw(:types);

    while ($xml->read) {
        next unless $xml->nodeType == XML_READER_TYPE_ELEMENT;
        my $name = $xml->name;

        if ($name eq "file_version") {
            my $version = $xml->readInnerXml;
            die "unsupported file_version $version (expected 4)\n"
                unless $version == 4;
        } elsif ($name eq "date_of_creation") {
            $self->{meta_insert}->execute("KanjiDic2", "created",
                                          $xml->readInnerXml);
            last;
        }
    }

    my %kanji;
    my $set_xml = sub {
        $kanji{$_[0]} = $xml->readInnerXml;
    };

    my %parser = (
        literal      => sub { $self->_process_kanji(\%kanji) if %kanji;
                              %kanji = ( literal => $xml->readInnerXml ); },
        rad_value    => sub { my $type = $xml->getAttribute("rad_type");
                              $set_xml->(@_) if $type eq "classical"; },
        grade        => $set_xml,
        stroke_count => sub { $set_xml->(@_)
                                  unless exists $kanji{stroke_count}; },
        freq         => $set_xml,
        variant      => sub { return if $xml->getAttribute("var_type") ne "ucs";
                              push(@{$kanji{variant}},
                                   chr hex $xml->readInnerXml); },
        reading      => sub { my $type = $xml->getAttribute("r_type");
                              if ($type =~ /^ja_(on|kun)$/) {
                                  push @{$kanji{$1}}, $xml->readInnerXml;
                              } },
        meaning      => do {
            if ($self->{lang}) {
                sub { my $lang = $xml->getAttribute("m_lang") // "en";
                      if (exists $self->{lang}->{$lang}) {
                          $xml->read;
                          my $text = $xml->value;
                          $text =~ tr/\t/ /;
                          push @{$kanji{meaning}}, $text;
                      } };
            } else {
                sub { $xml->read;
                      my $text = $xml->value;
                      $text =~ tr/\t/ /;
                      push @{$kanji{meaning}}, $text; };
            }
        },
        nanori       => sub { push @{$kanji{nanori}}, $xml->readInnerXml; },
        _END         => sub { $self->_process_kanji(\%kanji) if %kanji; }
    );

    return \%parser;
}


sub _process_kanji {
    my $self = shift;
    my ($kanji) = @_;

    my $kj = $kanji->{literal};
    my $kc = ord($kj);

    # Save seen kanji data for Unihan parser.
    $self->{kanji}->{$kc} = \my %data;

    if (exists $kanji->{on}) {
        my $tx = join "\t", @{$kanji->{on}};
        $self->{kanji_insert}->execute($kc, 0, $tx);

        @{$data{0}}{ @{$kanji->{on}} } = ();
    }

    if (exists $kanji->{kun}) {
        my $tx = join "\t", @{$kanji->{kun}};
        $self->{kanji_insert}->execute($kc, 1, $tx);

        foreach my $tx (@{$kanji->{kun}}) {
            my ($stem, $okurigana) = split /\./, $tx, 2;
            $data{1}{$stem} = undef;
            $data{1}{"$stem$okurigana"} = undef if $okurigana;
        }
    }

    if (exists $kanji->{nanori}) {
        my $tx = join "\t", @{$kanji->{nanori}};
        $self->{kanji_insert}->execute($kc, 2, $tx);

        @{$data{1}}{ @{$kanji->{nanori}} } = ();
    }

    if (exists $kanji->{meaning}) {
        my $tx = join "\t", @{$kanji->{meaning}};
        $self->{kanji_insert}->execute($kc, 3, $tx);

        @{$data{meaning}}{ map { fc } @{$kanji->{meaning}} } = ();
    }

    if (exists $kanji->{rad_value}) {
        $self->{kanji_insert_int}->execute($kc, 4, $kanji->{rad_value});

        $data{radical} = $kanji->{rad_value};
    }

    if (exists $kanji->{stroke_count}) {
        $self->{kanji_insert_int}->execute($kc, 5, $kanji->{stroke_count});
    }

    if (exists $kanji->{freq}) {
        $self->{kanji_insert_int}->execute($kc, 6, $kanji->{freq});
    }

    if (exists $kanji->{grade}) {
        $self->{kanji_insert_int}->execute($kc, 7, $kanji->{grade});
    }

    if (exists $kanji->{variant}) {
        my $tx = join "", @{$kanji->{variant}};
        $self->{kanji_insert}->execute($kc, 8, $tx);
    }
}


sub _populate {
    my $self = shift;

    $self->SUPER::_populate;

    my $english = !$self->{lang} || exists $self->{lang}->{en};
    my ($def_kc, $def) = (0);
    while (my $line = $self->{unihan_readings}->getline) {
        utf8::decode($line);
        if ($line =~ /^\s*(?:#|$)/) {
            if ($line =~ /^\s*#\s*(?:Date:\s*(\S+)|Unicode version:\s*(\S+))/) {
                $self->{meta_insert}->execute("Unihan", "created", $1) if $1;
                $self->{meta_insert}->execute("Unihan", "version", $2) if $2;
            }
            next;
        }
        my ($uc, $fi, $tx) = split /\t/, $line, 3;
        my $kc = hex substr($uc, 2);

        # Fields are sorted so kDefinition always comes before kJapanese...
        if ($fi eq "kDefinition" && $english) {
            if (exists $self->{kanji}->{$kc} || $tx =~ /\(J\)/) {
                $self->{kanji}->{$kc} //= {};  # Auto-vivify {$kc}.

                $self->_insert_meanings($kc, $tx);
            } else {
                # We are not sure yet that the character is used in
                # Japanese so save definition until we see the reading.
                $def_kc = $kc;
                $def = $tx;
            }
        } elsif ($fi eq "kJapaneseOn" || $fi eq "kJapaneseKun") {
            $self->{kanji}->{$kc} //= {};  # Auto-vivify {$kc}.

            my $tp = $fi eq "kJapaneseOn" ? 0 : 1;

            my $data = $self->{kanji}->{$kc};

            use GeekJDict::Romaji qw(jconvert);
            my @reading = split /\s+/, jconvert(2 - $tp, lc $tx);
            @reading = grep { !exists $data->{$tp}->{$_} } @reading;
            if (@reading) {
                my $tx = join "\t", @reading;
                if (%{$data->{$tp}}) {
                    $self->{kanji_append}->execute($kc, $tp, "\t$tx");
                } else {
                    $self->{kanji_insert}->execute($kc, $tp, $tx);
                }
            }

            if ($def_kc == $kc) {
                $self->_insert_meanings($kc, $def);
                $def_kc = 0;
            }
        }
    }

    # At this point $self->{kanji} contains all the kanji we care about.

    # First add Cangjie codes for basic forms themselves.
    while (readline $self->{cangjie5}) {
        if (/^\s*SERIAL_NUMBER\s*=\s*(\d{4})(\d\d)(\d\d)/) {
            $self->{meta_insert}->execute("Cangjie5", "created", "$1-$2-$3");
        }
        if (my $range = (/^\s*BEGIN_CHAR_PROMPTS_DEFINITION\s*$/
                         .../^\s*END_CHAR_PROMPTS_DEFINITION\s*$/)) {
            next if $range == 1 || $range =~ /E0$/ || /^[xz]/;

            my ($tx, $kc) = split /\s+/;
            $tx = uc $tx;
            $kc = ord $kc;
            $self->{kanji}->{$kc}->{cangjie}->{$tx} = undef;
            $self->{cangjie_insert}->execute($kc, 0, $tx);
        }
        last if /^\s*BEGIN_TABLE\s*$/;
    }

    # Add Cangjie codes from cangjie5.txt that do not contain 'X'.
    my $cangjie5_table = tell($self->{cangjie5});
    while (readline $self->{cangjie5}) {
        last if  /^\s*END_TABLE\s*$/;
        next if /x/;
        my ($tx, $kc) = split /\s+/;
        $tx = uc $tx;
        $kc = ord $kc;

        next unless exists $self->{kanji}->{$kc};

        my $cangjie = \$self->{kanji}->{$kc}->{cangjie};
        next if exists $$cangjie->{$tx};

        $$cangjie->{$tx} = undef;
        $self->{cangjie_insert}->execute($kc, scalar(keys %$$cangjie), $tx);
    }

    while (my $line = $self->{unihan_codes}->getline) {
        next if $line =~ /^\s*(?:#|$)/;
        my ($uc, $fi, $tx) = split /\t/, $line, 3;
        my $kc = hex substr($uc, 2);

        next unless exists $self->{kanji}->{$kc};

        if ($fi eq "kCangjie") {
            # Add Cangjie code from Unihan unless it already came from
            # cangjie5.txt or contains 'X' and "matches" some non-X
            # code for the same kanji (e.g. XABXC "matches" ABCDE).
            # In the latter case all 'X' are removed from the code.
            my $type = 0;
            my $cangjie = \$self->{kanji}->{$kc}->{cangjie};
            if ($tx !~ /X/) {
                next if exists $$cangjie->{$tx};
            } else {
                my $re = $tx =~ s/X/.*/gr;
                $re = qr/^$re/;
                next if grep { $_ =~ $re } keys %$$cangjie;
                $type = 8;
                $tx =~ s/X//g;
            }
            $$cangjie->{$tx} = undef;
            $self->{cangjie_insert}->execute($kc, $type | keys %$$cangjie, $tx);
        } elsif ($fi eq "kTotalStrokes"
                 && !exists $self->{kanji}->{$kc}->{radical}) {
            $tx =~ s/\s.*//;  # Use only first value.
            $self->{kanji_insert_int}->execute($kc, 5, $tx);
        }
    }

    # Finally add Cangjie codes with 'X' from cangjie5.txt with 'X'
    # removed, unless they "match" some code already added.
    seek($self->{cangjie5}, $cangjie5_table, 0);
    while (readline $self->{cangjie5}) {
        last if  /^\s*END_TABLE\s*$/;
        next unless /x/;
        my ($tx, $kc) = split /\s+/;
        $tx = uc $tx;
        $kc = ord $kc;

        next unless exists $self->{kanji}->{$kc};

        my $cangjie = \$self->{kanji}->{$kc}->{cangjie};
        next if exists $$cangjie->{$tx};

        my $re = $tx =~ s/X/.*/gr;
        $re = qr/^$re/;
        next if grep { $_ =~ $re } keys %$$cangjie;
        $tx =~ s/X//g;
        $$cangjie->{$tx} = undef;
        $self->{cangjie_insert}->execute($kc, 8 | keys %$$cangjie, $tx);
    }

    while (my $line = $self->{unihan_radical}->getline) {
        next if $line =~ /^\s*(?:#|$)/;
        my ($uc, $fi, $tx) = split /\t/, $line, 3;
        my $kc = hex substr($uc, 2);

        next unless exists $self->{kanji}->{$kc};
        my $data = $self->{kanji}->{$kc};
        next if exists $data->{radical};

        # kRSJapanese comes before kRSKangXi and takes priority over it.
        if (($fi eq "kRSJapanese" || $fi eq "kRSKangXi")
            && !exists $data->{radical}) {
            ($data->{radical}) = split /\./, $tx, 2;  # Use only first value.
            $self->{kanji_insert_int}->execute($kc, 4, $data->{radical});
        }
    }

    # TODO: Unihan_IRGSources.txt:kRSUnicode
    # TODO: Unihan_IRGSources.txt:kCompatibilityVariant (?)
    # TODO: Unihan_Variants.txt
}


sub _insert_meanings {
    my $self = shift;
    my ($kc, $def) = @_;

    my $data = $self->{kanji}->{$kc};

    if ($kc == 0x58F2) {
        # Unicode 9.0.0 for 売 had a record
        #   U+58F2  kDefinition     sell; [NOT casing, shell, husk]
        # which is probably a leak out of a temporal note.
        $def =~ s/;\s*\[NOT[^]]*\]//;
    }

    # Leave only Japanese-related part.
    $def =~ s/^.*\(J\)\s*//;
    $def =~ s/\(Cant\.\).*//;

    # Remove Unicode references.
    $def =~ s/\s*U\+?[[:xdigit:]]+//g;
    $def =~ s/\(\s*\)|\[\s*\]//g;

    # Split on separators (unless they are in parenthesis).
    my @meaning = $def =~ /\G((?:\([^)]*\)|[^(,;]+)+)(?:[,;]+\s*|$)/g;

    # Filtering is naïve and won't filter out changed wording (e.g. 㡡).
    @meaning = grep { !exists $data->{meaning}->{fc $_} } @meaning;

    if (@meaning) {
        # According to http://www.unicode.org/reports/tr38/#kDefinition
        # there are no tabs in kDefinition.
        my $tx = join "\t", @meaning;
        if (%{$data->{meaning}}) {
            $self->{kanji_append}->execute($kc, 3, "\t$tx");
        } else {
            $self->{kanji_insert}->execute($kc, 3, $tx);
        }
    }
}


sub _finalize {
    my $self = shift;

    my $dbh = $self->{dbh};

    # Set absent Cangjie code of kanji z-variants to the codes of
    # their counterparts (i.e. the kanji they are the variant of).
    use Unicode::Normalize;
    use DBD::SQLite;
    $dbh->sqlite_create_function("NFKC", 1, \&Unicode::Normalize::NFKC,
                                 DBD::SQLite::Constants::SQLITE_DETERMINISTIC);
    $dbh->do(q{
        INSERT INTO cangjie (kc, ti, tx)
        SELECT DISTINCT k.kc, c.ti, c.tx
        FROM kanji AS k JOIN cangjie AS c
        WHERE k.kc <> c.kc AND unicode(NFKC(char(k.kc))) = c.kc
            AND NOT EXISTS (SELECT * FROM cangjie WHERE kc = k.kc)
    });

    $self->_limit_cangjie
        if $dbh->tables(undef, undef, "word", "TABLE");
}


1
