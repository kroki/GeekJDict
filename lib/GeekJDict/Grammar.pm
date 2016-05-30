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
package GeekJDict::Grammar;

use v5.16;
use strict;
use warnings qw(all);
use utf8;


use GeekJDict::Util qw(globs2regexps);


sub new {
    my $class = shift;
    my ($grammar, $dbh) = @_;

    my $self = bless { dbh => $dbh }, $class;

    $self->_prepare_statements;
    $self->_load_grammar($grammar);

    return $self;
}


sub _prepare_statements {
    my $self = shift;

    my $dbh = $self->{dbh};
    $self->{word} = $dbh->prepare(q{
        SELECT wi
        FROM term_index
        WHERE tx = ?
    });
    $self->{meta} = $dbh->prepare(q{
        SELECT group_concat(mr, '')
        FROM word
        WHERE id = ?1 AND it & 7 = 2
            AND EXISTS (SELECT *
                        FROM word
                        WHERE id = ?1 AND it & 7 < 2 AND tx = ?2)
    });
}


sub _word_tags {
    my $self = shift;
    my ($word) = @_;

    my $dbh = $self->{dbh};

    my $tags;
    my $wi = $dbh->selectrow_array($self->{word}, undef, $word);
    if (defined $wi) {
        my $id = 0;
        foreach my $d (unpack "w*", $wi) {
            $id += $d;
            my $mr = $dbh->selectrow_array($self->{meta}, undef, $id, $word);
            if (defined $mr) {
                foreach my $i (unpack "U*", $mr) {
                    vec($tags, $i, 1) = 1;
                }
            }
        }
    }

    return $tags;
}


my %sound_change = (
    "" => [""],
    ア => [qw(さ か が ま ば な ら わ た)],  # Note あ -> わ.
    イ => [qw(し き ぎ み び に り い ち)],
    ウ => [qw(す く ぐ む ぶ ぬ る う つ)],
    エ => [qw(せ け げ め べ ね れ え て)],
    オ => [qw(そ こ ご も ぼ の ろ お と)]
);

sub _load_grammar {
    my $self = shift;
    my ($grammar) = @_;

    my $dbh = $self->{dbh};
    my %tags = @{$dbh->selectcol_arrayref(q{
        SELECT ab, id
        FROM word_meta
        WHERE ki = 'pos'
    }, { Columns => [1, 2] })};

    my %backward = (plain => []);

    open(my $fh, '<:utf8', $grammar)
        or die "Can't open grammar file $grammar: $!\n";
    local $SIG{__DIE__} = sub { die "$grammar:$.: ", @_ };
    while (<$fh>) {
        chomp;
        s/#.*//;
        next if /^\s*$/;
        my ($nform, $rform, $ns, $npat, $ntag, $oform, $os, $opat, $oglobs) =
            /^\s*(\S+) \s+(?:(\S+) \s+)?〜([アイウエオ]?)(\S*) \s+(?:(\S+) \s+)?
             for \s+(\S+) \s+〜([アイウエオ]?)(\S*) \s+(\S+ (?:\s+\S+)*) \s*$/xi
            or die "can't parse line '$_'\n";

        die "rule for '$oform' hasn't been defined yet\n"
            unless exists $backward{$oform};

        if (defined $ntag) {
            $ntag = $tags{$ntag} or die "no such tag '$ntag'\n";
        }

        my @regexps = map { qr/^$_$/ } globs2regexps($oglobs);
        my $otags;
        for (my $i = 0; $i < @regexps; ++$i) {
            my @m = grep { $_ =~ $regexps[$i] } keys %tags;
            unless (@m) {
                my $glob = (split /\s+/, $oglobs)[$i];
                die "'$glob' doesn't match any tag\n";
            }
            foreach my $t (map { $tags{$_} } @m) {
                vec($otags, $t, 1) = 1;
            }
        }

        die "sound katakana should be in both patterns\n"
            unless !$ns == !$os;

        my $nsound = $sound_change{$ns};
        my $osound = $sound_change{$os};
        for (my $i = 0; $i < @$nsound; ++$i) {
            my $rule = [$nform, $ntag, qr/$nsound->[$i]$npat$/,
                        $oform, $otags, "$osound->[$i]$opat"];
            push @{$backward{""}}, $rule;
            push @{$backward{$nform}}, $rule;
            push @{$backward{$rform}}, $rule if defined $rform;
        }
    }

    my %itags;
    @itags{ values %tags } = keys %tags;

    $self->{backward} = \%backward;
    foreach my $id (values %tags) {
        vec($self->{tags}, $id, 1) = 1;
    }
    $self->{itags} = \%itags;
}


sub _tag_string {
    my $self = shift;
    my ($tags) = @_;

    my $itags = $self->{itags};

    my @tags;
    my $bits = length($tags) * 8;
    for (my $i = 0; $i < $bits; ++$i) {
        push @tags, $itags->{$i} if vec($tags, $i, 1);
    }

    return join " ", sort @tags;
}


sub infer_backward {
    my $self = shift;
    my ($query) = @_;

    $query =~ s/\s.*//;

    my $node = [$query];
    $self->_infer_backward($node, $self->{tags}, "", {});

    return $node;
}


sub _infer_backward {
    my $self = shift;
    my ($node, $tags, $form, $seen) = @_;

    if (keys %$seen >= 50) {
        push @$node, ["https://github.com/kroki/GeekJDict/issues:",
                      "grammar rule loop detected"];
        return;
    }

    my $word = $node->[0];

    return if exists $seen->{$word};
    local $seen->{$word} = undef;

    my $rule_tags = !$form || $form eq "plain" ? $tags : "";
    foreach my $r (@{$self->{backward}->{$form}}) {
        my $w = $word;
        $w =~ s/$r->[2]/$r->[5]/ and $w ne "" or next;
        my $t;
        if (defined $r->[1]) {
            next unless vec($tags, $r->[1], 1);
            $t = $r->[4];
            vec($rule_tags, $r->[1], 1) = 1;
        } else {
            $t = $tags & $r->[4];
            next if $t =~ /^\0*$/;
            $rule_tags |= $t;
        }
        my $n = [$w, $r->[0]];
        $self->_infer_backward($n, $t, $r->[3], $seen);
        push @$node, $n if @$n > 2;
    }

    if ($rule_tags) {
        my $word_tags = $self->_word_tags($word);
        if (defined $word_tags) {
            my $tag_string = $self->_tag_string($word_tags & $rule_tags);
            push @$node, [$form, $tag_string] if $tag_string;
        }
    }
}


1
