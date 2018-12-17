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
package GeekJDict::Util;
use parent "Exporter";

use v5.16;
use strict;
use warnings qw(all);
use utf8;


BEGIN {
    our @EXPORT_OK = qw(get_terms get_globs globs2regexps);
}


sub _normalize {
    use Unicode::Normalize;
    # To avoid digraph expansion during normalization first replace
    # them with dummies from Private Use Area (U+E000..U+F8FF).
    $_[0] =~ tr/ゟヿ/\x{e000}\x{e001}/;
    # Fold case and remove diacritical marks.
    $_[0] = NFKD fc $_[0];
    $_[0] =~ s/\p{Block: Combining_Diacritical_Marks}//g;
    $_[0] = NFC $_[0];
    # Restore digraphs.
    $_[0] =~ tr/\x{e000}\x{e001}/ゟヿ/;

    # Remove "・" separators in katakana.
    $_[0] =~ s/・//g;

    # Remove commas in numbers.
    $_[0] =~ s/([0-9]),(?=[0-9])/$1/g;

    # Instantiate repetitions.
    $_[0] =~ s/(.)々/$1$1/g;

    return $_[0];
}


sub get_terms {
    return _normalize($_[0]) =~ /((?:[^\W_]|[⻌○×◎〃※〒△→←↑↓])+)/g;
}


sub get_globs {
    return _normalize($_[0]) =~ /((?:[^\W_]|[⻌○×◎〃※〒△→←↑↓]|[]^[?*-])+)/g;
}


sub globs2regexps {
    my ($input) = @_;

    my @globs = split /\s+/, $input;
    foreach my $glob (@globs) {
        my @glob;
        my $cset = 0;
        foreach my $g (split /([][?*^-])/, $glob) {
            next if $g eq "";
            if ($g eq "]") {
                $cset = 0
                    unless ($cset == @glob
                            || ($cset == @glob - 1 && $glob[$cset] eq "^"));
            } elsif ($g eq "[") {
                unless ($cset) {
                    $cset = @glob + 1;
                    $g = "(?=\\S)[";
                }
            } elsif ($g eq "?") {
                $g = "\\S" unless $cset;
            } elsif ($g eq "*") {
                $g = "\\S*" unless $cset;
            } elsif ($g eq "^") {
                $g = "\\^" unless $cset;
            } elsif ($g ne "-") {
                $g =~ s/([.+()|\$\\])/\\$1/g;
            }
            push @glob, $g;
        }
        unless ($cset) {
            $glob = join "", @glob;
        } else {
            $glob = "(*FAIL)";
        }
    }

    return @globs;
}


1
