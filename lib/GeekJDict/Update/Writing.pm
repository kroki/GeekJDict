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
package GeekJDict::Update::Writing;
use parent "GeekJDict::Update";

use v5.16;
use strict;
use warnings qw(all);


sub new {
    my $class = shift;
    my ($option, $kanjivg) = @_;

    return $class->SUPER::new($option, $kanjivg);
}


sub _create_tables {
    my $self = shift;

    $self->set_db_version("writing");

    my $dbh = $self->{dbh};

    # writing_meta table:
    #  id - row ID.
    #  ki - record kind.
    #  ky - record key.
    #  vl - record value.
    $dbh->do(q{ DROP TABLE IF EXISTS writing_meta });
    $dbh->do(q{
        CREATE TABLE writing_meta (
            id INTEGER PRIMARY KEY,
            ki TEXT NOT NULL,
            ky TEXT NOT NULL,
            vl TEXT NOT NULL
        )
    });
    $self->{meta_insert} = $dbh->prepare(q{
        INSERT INTO writing_meta (ki, ky, vl)
        VALUES (?, ?, ?)
    });

    # writing table:
    #  cc - character code point.
    #  sn - stroke number.
    #  sc - stroke curves.  First all 'c', 's' and 'S' SVG commands
    #       are converted to 'C' commands.  After that we have 1+3N
    #       absolute coordinate pairs in [0--109] range each, for N
    #       Bézier curves (first point originating from initial 'M'
    #       (or 'm') command).  Then we replace all coordinates after
    #       the first pair with the difference relative to previous
    #       pair, and translate first coordinate pair by (-55,-55)
    #       (i.e. 58.32,24.65 63.17,21.04 57.43,22.00 ... becomes
    #       3.32,-30.35 4.85,-3.61 -5.74,0.96 ...).  The we scale
    #       coordinates by 10 and round to nearest integers (previous
    #       sequence becomes 33,-303 49,-36 -57,10 ...).  Finally we
    #       map positive numbers to even naturals and negative numbers
    #       to odd naturals (getting 66,605 98,71 113,20 ...), and as
    #       a last step pack the sequence with Perl's pack "w*".
    $dbh->do(q{ DROP TABLE IF EXISTS writing });
    $dbh->do(q{
        CREATE TABLE writing (
            cc INTEGER NOT NULL,
            sn INTEGER NOT NULL,
            sc BLOB NOT NULL,

            PRIMARY KEY (cc, sn)
        ) WITHOUT ROWID
    });
    $self->{writing_insert} = $dbh->prepare(q{
        INSERT INTO writing (cc, sn, sc)
        VALUES (?, ?, ?)
    });
}


sub _init_parser {
    my $self = shift;

    my $xml = $self->{xml};

    use XML::LibXML::Reader qw(:types);

    while ($xml->read && $xml->nodeType == XML_READER_TYPE_COMMENT) {
        if ($xml->value =~ /generated on (\d{4}-\d\d-\d\d)/) {
            $self->{meta_insert}->execute("KanjiVG", "created", $1);
        }
    }

    my ($cc, $sn);
    my $insert_stroke = sub {
        # We don't need writings of Latin letters and digits.
        return if ($cc < 128
                   || chr($cc) =~ /\p{Block: Halfwidth_And_Fullwidth_Forms}/);

        my $d = $xml->getAttribute("d");
        my @svg = $d =~ /([a-zA-Z]|[+-]?(?:\d+(?:\.\d*)?|\.\d+))/g;

        my @sc;
        my $cmd = shift @svg;
        if ($cmd !~ /[Mm]/) {
            die("At line ", $xml->lineNumber,
                ": expected first SVG command to be 'M', got '$cmd'\n");
        }
        push @sc, splice(@svg, 0, 2);
        $cmd = shift @svg;
        do {
            if ($cmd !~ /[CcSs]/) {
                die("At line ", $xml->lineNumber,
                    ": expected SVG command to be 'C' or 'S', got '$cmd'\n");
            }
            my $count = $cmd =~ /[Cc]/ ? 6 : 4;
            if (@svg < $count) {
                die("At line ", $xml->lineNumber,
                    ": expected $count coordinates, remains less\n");
            }
            my @c = splice(@svg, 0, $count);
            if ($cmd =~ /[cs]/) {
                for (my $i = 0; $i < @c; $i += 2) {
                    $c[$i] += $sc[-2];
                    $c[$i + 1] += $sc[-1];
                }
            }
            if ($cmd =~ /[Ss]/) {
                if (@sc > 2) {
                    unshift @c, 2 * $sc[-2] - $sc[-4], 2 * $sc[-1] - $sc[-3];
                } else {
                    unshift @c, $sc[-2], $sc[-1];
                }
            }
            push @sc, @c;
            $cmd = shift @svg if @svg && $svg[0] =~ /[a-zA-Z]/;
        } while (@svg);

        for (my $i = @sc - 2; $i > 0; $i -= 2) {
            $sc[$i] -= $sc[$i - 2];
            $sc[$i + 1] -= $sc[$i - 1];
        }

        $sc[0] -= 55;
        $sc[1] -= 55;
        foreach my $c (@sc) {
            $c = int($c * 10 + 0.5) * 2;
            $c = -$c - 1 if $c < 0;
        };

        $self->{writing_insert}->bind_param(1, $cc, DBI::SQL_INTEGER);
        $self->{writing_insert}->bind_param(2, ++$sn, DBI::SQL_INTEGER);
        $self->{writing_insert}->bind_param(3, pack("w*", @sc), DBI::SQL_BLOB);
        $self->{writing_insert}->execute;
    };
    my %parser = (
        kanji => sub { my $id = $xml->getAttribute("id");
                       $id =~ s/.*_//;
                       $cc = hex($id);
                       $sn = 0; },
         path => $insert_stroke,
        _END  => sub {}
    );

    return \%parser;
}


1
