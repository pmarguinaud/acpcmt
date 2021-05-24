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

    my @end = &f ('./f:C[text ()="!$acc end kernels"]', $pu);

    for my $end (@end)
      {
        my @n = &f ('following-sibling::node ()', $end);
        for my $n (@n)
          {
            if (($n->nodeName eq 'C') && ($n->textContent eq '!$acc kernels'))
              {
                $end->unbindNode ();
                $n->unbindNode ();
                last;
              }
            last unless ($n->nodeName eq '#text');
          }
      }



  }


'FileHandle'->new (">$F90.new")->print ($doc->textContent);
