# GeekJDict

GeekJDict is a text-mode Japanese dictionary with powerful input and
lookup capabilities.  To run it requires the terminal that supports
UTF-8 and at least 16 ANSI colors (`w` command requires 256 ANSI
colors), and terminal font that has Japanese glyphs.  Most terminal
emulators in graphic desktop environments of modern OS distributions
comply with these requirements out of the box.

GeekJDict can provide dictionary information about Japanese words and
expressions, individual kanji, show handwriting of selected
characters, and trace inflected words to their dictionary form.
GeekJDict implements powerful glob pattern lookup, direct hiragana and
katakana input, as well as direct kanji input via built-in support of
[Cangjie input method]
(https://en.wikipedia.org/wiki/Cangjie_input_method) with glob
patterns and completion.


## Installation

To install GeekJDict first copy source files to your host (by cloning
Git repository or downloading and unpacking Zip archive).  Then
optionally create a symlink to `geekjdict` program from any directory
listed in your `$PATH` so that you won't have to type a full path every
time you run GeekJDict.

GeekJDict is written in Perl programming language and requires the
following Perl modules to be installed (Fedora package names are given
in parenthesis; other distributions may name them differently):

    Archive::Zip               (perl-Archive-Zip)
    DBD::SQLite                (perl-DBD-SQLite)
    DBI                        (perl-DBI)
    Imager                     (perl-Imager)
    PerlIO::gzip               (perl-PerlIO-gzip)
    Term::ReadLine::Gnu        (perl-Term-ReadLine-Gnu)
    XML::LibXML::Reader        (perl-XML-LibXML)

The following Perl modules are listed in `perlmodlib` as standard,
however you may have to install separate packages for some of them
depending on your OS distribution:

    FindBin                    (installed with Perl)
    Getopt::Long               (perl-Getopt-Long)
    Locale::Codes::Language    (perl-Locale-Codes)
    Pod::Usage                 (perl-Pod-Usage)
    Term::ANSIColor            (perl-Term-ANSIColor)
    Term::ReadLine             (installed with Perl)
    Unicode::Normalize         (perl-Unicode-Normalize)
    open                       (perl-open)
    parent                     (perl-parent)


## Bootstrapping

Before using GeekJDict you have to populate dictionary database.

First you need to download the following files (use right mouse button
in the browser and choose "Save as..."):

* [kanjidic2.xml.gz] (http://www.edrdg.org/kanjidic/kanjidic2.xml.gz)
* [Unihan.zip] (http://www.unicode.org/Public/UCD/latest/ucd/Unihan.zip)
* [cangjie5.txt] (https://raw.githubusercontent.com/definite/ibus-table-chinese/master/tables/cangjie/cangjie5.txt)
* `kanjivg-VERSION.xml.gz` (`VERSION` stands for some date) from [here]
  (https://github.com/KanjiVG/kanjivg/releases/latest)
* JMdict.gz from ftp://ftp.monash.edu.au/pub/nihongo/JMdict.gz

Then execute the following commands:

    $ geekjdict --update=kanji --lang=eng kanjidic2.xml.gz Unihan.zip cangjie5.txt
    $ geekjdict --update=writing kanjivg-VERSION.xml.gz
    $ geekjdict --update=words --lang=eng JMdict.gz

You may use additional `--lang` options, or omit `--lang` entirely to
get all available languages.


## Running

To make the most out of GeekJDict features you are advised to read the
output of `geekjdict --help`, especially sections on kana and kanji
input, glob pattern queries and grammar inferences.


# License

Copyright (C) 2016 Tomash Brechko. All rights reserved.

GeekJDict is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your option)
any later version.

GeekJDict is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
more details.

You should have received a copy of the GNU General Public License along
with GeekJDict. If not, see <http://www.gnu.org/licenses/>.


## Data

GeekJDict source distribution doesn't contain any data files. However
GeekJDict is assumed to be used with the following data:

[JMDICT] (http://www.edrdg.org/jmdict/j_jmdict.html) and [KANJIDIC2]
(http://www.csse.monash.edu.au/~jwb/kanjidic2/) dictionary files are
the property of the [Electronic Dictionary Research and Development
Group] (http://www.edrdg.org), and are used in conformance with the
Group's [licence] (http://www.edrdg.org/edrdg/licence.html).

[KanjiVG] (http://kanjivg.tagaini.net/index.html) data files are
copyright © 2009-2015 Ulrich Apel and released under the Creative
Commons Attribution-Share Alike 3.0 [license]
(http://creativecommons.org/licenses/by-sa/3.0/).

[Unihan] (http://www.unicode.org/charts/unihan.html) data files are
copyright © 1991-2016 Unicode, Inc. and distributed under the [Terms
of Use] (http://www.unicode.org/copyright.html).

[cangjie5.txt]
(https://github.com/definite/ibus-table-chinese/tree/master/tables/cangjie)
data file is freely redistributable without restriction.

See `license/` directory in GeekJDict source tree for further details.
