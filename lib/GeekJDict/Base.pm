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
package GeekJDict::Base;

use v5.16;
use strict;
use warnings qw(all);
use utf8;


sub new {
    my $class = shift;
    my ($db, $mode) = @_;

    use DBI;
    use DBD::SQLite;

    my $dsn = "DBI:SQLite:dbname=file:$db?mode=$mode";
    my $attrs = { AutoCommit => 1,
                  PrintError => 0,
                  RaiseError => 1,
                  sqlite_unicode => 1,
                  sqlite_open_flags => DBD::SQLite::OPEN_URI };
    my $dbh = DBI->connect($dsn, '', '', $attrs);
    $dbh->do(q{
        PRAGMA page_size = 4096
    });
    $dbh->do(q{
        PRAGMA mmap_size = 4294967296
    });

    return bless { dbh => $dbh }, $class;
}


1
