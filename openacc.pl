#!/usr/bin/perl -w
#
use strict;
use FileHandle;
use Data::Dumper;

use FindBin qw ($Bin);
use lib $Bin;
use Fxtran;

my $F90 = shift;

my $doc = &Fxtran::fxtran (location => $F90);

my @pu = &f ('./f:object/f:file/f:program-unit', $doc);

for my $pu (@pu)
  {
    my @do = &f ('.//f:do-construct[./f:do-stmt/f:do-V/f:named-E/f:N/f:n/text()="JBLK"]', $doc);

    for my $do (@do)
      {
        my %p;

        # Loop variables & temporary scalars
        my @s = &f ('.//f:E-1/f:named-E[not (./f:R-LT)]/f:N/f:n/text ()', $do);
        my @v = &f ('descendant-or-self::f:do-construct/f:do-stmt/f:do-V/f:named-E/f:N/f:n/text()', $do);
     
        for (@s, @v)
          {
            $p{$_->textContent}++;
          }

        my @p = sort keys (%p);

        # KLON arrays
        
        my @a = &f ('.//f:named-E[./f:R-LT/f:parens-R/f:element-LT/f:element/f:named-E/f:N/f:n/text ()="JLON"]'
                  . '/f:N/f:n/text ()', $do);

#       print &Dumper ([map { $_->textContent } @a]);
        my %a = map { ($_->textContent, 1) } @a;
        @a = sort keys (%a);

        my $sp = $do->previousSibling;
        ($sp = $sp->textContent) =~ s/^\s*\n//o;
        $do->parentNode->insertBefore (&n ('<C>!$acc parallel loop gang vector collapse (2) vector_length (KLON) '
                                         . (@p ? 'private (' . join (', ', @p) . ') ' : '')
#                                        . (@a ? 'present (' . join (', ', @a) . ') ' : '')
                                         . '</C>'), $do);
        $do->parentNode->insertBefore (&t ("\n"), $do);
        $do->parentNode->insertBefore (&t ($sp), $do);
      }


  }

'FileHandle'->new (">$F90.new")->print ($doc->textContent);
