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
package GeekJDict::Update;
use parent "GeekJDict::Base";

use strict;
use warnings qw(all);


sub new {
    my $class = shift;
    my ($option, $xml, @xml_args) = @_;

    open(my $fh, "<:gzip", $xml)
        or die "Can't open $xml: $!\n";

    use XML::LibXML::Reader;

    my $reader = XML::LibXML::Reader->new(
        IO => $fh,
        no_network => 1,
        no_blanks => 1,
        no_cdata => 1,
        @xml_args
    );

    my $self = $class->SUPER::new($option->{db}, "rwc");

    $self->{xml} = $reader;

    return $self;
}


sub run {
    my $self = shift;

    my $dbh = $self->{dbh};

    $dbh->begin_work;

    $self->_create_tables;
    $self->_populate;
    $self->_finalize;

    $dbh->commit;

    $dbh->do(q{ VACUUM });
}


sub _populate {
    my $self = shift;

    my $xml = $self->{xml};

    use XML::LibXML::Reader qw(:types);

    my $parser = $self->_init_parser;
    my $pattern =
        XML::LibXML::Pattern->new(join "|",
                                  grep { $_ ne "_END" } keys %$parser);
    while ($xml->nextPatternMatch($pattern)) {
        next if $xml->nodeType == XML_READER_TYPE_END_ELEMENT;

        my $name = $xml->name;
        $parser->{$name}->($name);
    }
    $parser->{_END}->();
}


sub _finalize {
}


sub _limit_cangjie {
    my $self = shift;

    my $dbh = $self->{dbh};

    my $select_words = $dbh->prepare(q{
        SELECT tx
        FROM word
        WHERE it & 7 = 0
    });
    $select_words->execute;
    $select_words->bind_col(1, \my $word);
    my %kanji;
    @kanji{ $word =~ /(\p{Han})/g } = () while $select_words->fetch;

    $dbh->do(q{
        UPDATE cangjie
        SET ti = ti | 16
        WHERE ti > 0
    });
    my $update_cangjie = $dbh->prepare(q{
        UPDATE cangjie
        SET ti = ti & ~16
        WHERE kc = ?
    });
    foreach my $kc (sort { $a <=> $b } map { ord } keys %kanji) {
        $update_cangjie->execute($kc);
    }
}


1
