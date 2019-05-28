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
package GeekJDict::Romaji;
use parent "Exporter";

use strict;
use warnings qw(all);
use utf8;


BEGIN {
    our @EXPORT_OK = qw(jconvert jconvert_last);
}


my @map = \my (%romaji, %hiragana, %katakana);


sub jconvert {
    my ($map, $text) = @_;

    $map = $map[$map];
    $text =~ s/(.{1,4})(?(?{ exists $map->{$1} })|(*FAIL))/ $map->{$1} /eg;

    return $text;
}


sub jconvert_last {
    my ($map, $text) = @_;

    $map = $map[$map];
    $text =~ s/(.{1,4})$(?(?{ exists $map->{$1} })|(*FAIL))/ $map->{$1} /e;

    return $text;
}


BEGIN {
    # Since we need lossless conversions romaji-hiragana-katakana both
    # ways we use Hepburn romanization
    # (https://en.wikipedia.org/wiki/Hepburn_romanization) mixed with
    # Nihon-shiki romanization
    # (https://en.wikipedia.org/wiki/Nihon-shiki_romanization), with
    # the following exceptions:
    #
    #   ぢ => dji (ji in Hepburn, di in Nihon-shiki)
    #   づ => dzu (zu in Hepburn, du in Nihon-shiki)
    #
    # We support ゐ (U+3090, wi), ゑ (U+3091, we), and ゔ (U+3094,
    # vu).  We do not support digraphs ゟ (U+309f, yori) and ヿ
    # (U+30ff, koto), as well as archaic 𛀁 (U+1b001, ye).
    %romaji = (
        # Plain.
        あ =>  "a", い =>   "i", う =>   "u", え =>  "e", お =>  "o",
        か => "ka", き =>  "ki", く =>  "ku", け => "ke", こ => "ko",
        さ => "sa", し => "shi", す =>  "su", せ => "se", そ => "so",
        た => "ta", ち => "chi", つ => "tsu", て => "te", と => "to",
        な => "na", に =>  "ni", ぬ =>  "nu", ね => "ne", の => "no",
        は => "ha", ひ =>  "hi", ふ =>  "fu", へ => "he", ほ => "ho",
        ま => "ma", み =>  "mi", む =>  "mu", め => "me", も => "mo",
        や => "ya",              ゆ =>  "yu",             よ => "yo",
        ら => "ra", り =>  "ri", る =>  "ru", れ => "re", ろ => "ro",
        わ => "wa", ゐ =>  "wi",              ゑ => "we", を => "wo",
        ん => "n",

        # Voiced.
        が => "ga", ぎ =>  "gi", ぐ =>  "gu", げ => "ge", ご => "go",
        ざ => "za", じ =>  "ji", ず =>  "zu", ぜ => "ze", ぞ => "zo",
        だ => "da", ぢ => "dji", づ => "dzu", で => "de", ど => "do",
        ば => "ba", び =>  "bi", ぶ =>  "bu", べ => "be", ぼ => "bo",
                                 ゔ =>  "vu",

        # Plosive.
        ぱ => "pa", ぴ =>  "pi", ぷ =>  "pu", ぺ => "pe", ぽ => "po",

        # ん as m before m, p, b.
        んま => "mma", んみ =>  "mmi", んむ =>  "mmu", んめ => "mme", んも => "mmo",
        んば => "mba", んび =>  "mbi", んぶ =>  "mbu", んべ => "mbe", んぼ => "mbo",
        んぱ => "mpa", んぴ =>  "mpi", んぷ =>  "mpu", んぺ => "mpe", んぽ => "mpo",

        # Prolonged sound mark.
        ー => "--"
    );
    # Diphthongs.
    foreach my $mora (qw(き       に ひ み り ぎ       び ぴ  んみ んび んぴ)) {
        $romaji{"${mora}ゃ"} = substr($romaji{$mora}, 0, -1) . "ya";
        $romaji{"${mora}ゅ"} = substr($romaji{$mora}, 0, -1) . "yu";
        $romaji{"${mora}ょ"} = substr($romaji{$mora}, 0, -1) . "yo";
    }
    foreach my $mora (qw(   し ち                じ ぢ      )) {
        $romaji{"${mora}ゃ"} = substr($romaji{$mora}, 0, -1) . "a";
        $romaji{"${mora}ゅ"} = substr($romaji{$mora}, 0, -1) . "u";
        $romaji{"${mora}ょ"} = substr($romaji{$mora}, 0, -1) . "o";
    }
    # Geminates.
    my %geminate;
    while (my ($mora, $romaji) = each %romaji) {
        if ($romaji =~ /^([kstcgzjdhfbp])/) {
            $geminate{"っ$mora"} = "$1$romaji";
        }
    }
    @romaji{keys %geminate} = values %geminate;
    # んあ => n'a, etc.
    foreach my $vowel (qw(あ い う え お  や ゆ よ)) {
        $romaji{"ん$vowel"} = "n'$romaji{$vowel}";
    }

    # Initialize hiragana.
    %hiragana = reverse %romaji;
    # んa => な, but ん'a => んあ, etc.
    foreach my $vowel (qw(a i u e o  ya yu yo)) {
        $hiragana{"ん$vowel"} = $hiragana{"n$vowel"};
        $hiragana{"ん'$vowel"} = "ん$hiragana{$vowel}";
    }

    # Initialize katakana.
    while (my ($romaji, $mora) = each %hiragana) {
        # In conversion from hiragana to katakana we depend on Unicode
        # layout, see http://www.unicode.org/charts/PDF/U3040.pdf and
        # http://www.unicode.org/charts/PDF/U30A0.pdf .
        $romaji =~ tr/\x{3041}-\x{3096}/\x{30a1}-\x{30f6}/;
        $mora =~   tr/\x{3041}-\x{3096}/\x{30a1}-\x{30f6}/;
        $katakana{$romaji} = $mora;
    }

    # Katakana digraphs from
    # https://en.wikipedia.org/wiki/Hepburn_romanization#For_extended_katakana
    # (blue take precedence over beige).
    $katakana{yi}   = "イィ";
    $katakana{ye}   = "イェ";
    #$katakana{wa}   = "ウァ";  # JMdict seems to use original wa and wo.
    $katakana{wi}   = "ウィ";
    $katakana{wu}   = "ウゥ";
    $katakana{we}   = "ウェ";
    #$katakana{wo}   = "ウォ";  # JMdict seems to use original wa and wo.
    $katakana{wyu}  = "ウュ";
    $katakana{va}   = "ヴァ";
    $katakana{vi}   = "ヴィ";
    #$katakana{vu}   = "ヴ";  # This will come from hiragana conversion.
    $katakana{ve}   = "ヴェ";
    $katakana{vo}   = "ヴォ";
    $katakana{vya}  = "ヴャ";
    $katakana{vyu}  = "ヴュ";
    $katakana{vye}  = "ヴィェ";
    $katakana{vyo}  = "ヴョ";
    $katakana{kye}  = "キェ";
    $katakana{gye}  = "ギェ";
    $katakana{kwa}  = "クヮ";
    $katakana{kwi}  = "クィ";
    $katakana{kwe}  = "クェ";
    $katakana{kwo}  = "クォ";
    $katakana{gwa}  = "グァ";
    $katakana{gwi}  = "グィ";
    $katakana{gwe}  = "グェ";
    $katakana{gwo}  = "グォ";
    $katakana{she}  = "シェ";
    $katakana{je}   = "ジェ";
    $katakana{si}   = "スィ";
    $katakana{zi}   = "ズィ";
    $katakana{che}  = "チェ";
    $katakana{tsa}  = "ツァ";
    $katakana{tsi}  = "ツィ";
    $katakana{tse}  = "ツェ";
    $katakana{tso}  = "ツォ";
    $katakana{tsyu} = "ツュ";
    $katakana{ti}   = "ティ";
    $katakana{tu}   = "トゥ";
    $katakana{tyu}  = "テュ";
    $katakana{di}   = "ディ";
    $katakana{du}   = "ドゥ";
    $katakana{dyu}  = "デュ";
    $katakana{nye}  = "ニェ";
    $katakana{hye}  = "ヒェ";
    $katakana{bye}  = "ビェ";
    $katakana{pye}  = "ピェ";
    $katakana{fa}   = "ファ";
    $katakana{fi}   = "フィ";
    $katakana{fe}   = "フェ";
    $katakana{fo}   = "フォ";
    $katakana{fya}  = "フャ";
    $katakana{fyu}  = "フュ";
    $katakana{fye}  = "フィェ";
    $katakana{fyo}  = "フョ";
    $katakana{hu}   = "ホゥ";
    $katakana{mye}  = "ミェ";
    $katakana{rye}  = "リェ";
    # Following five are not used in JMdcit (well, some of the above
    # too).  The problem with them is that fullwidth plosive mark
    # looks ugly, but halfwidth version is normalized not to fullwidth
    # version, but to combining version (U+309a), see
    # http://www.unicode.org/cldr/charts/latest/supplemental/character_fallback_substitutions.html
    #$katakana{la}   = "ラ゜"; # "ラﾟ";
    #$katakana{li}   = "リ゜"; # "リﾟ";
    #$katakana{lu}   = "ル゜"; # "ルﾟ";
    #$katakana{le}   = "レ゜"; # "レﾟ";
    #$katakana{lo}   = "ロ゜"; # "ロﾟ";

    # Finally add katakana to romaji map.
    while (my ($romaji, $mora) = each %katakana) {
        $romaji{$mora} = $romaji unless $romaji =~ /^[んン]/;
    }
}


1
