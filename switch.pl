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
    my @do_jlon = &f ('.//f:do-construct[./f:do-stmt/f:do-V/f:named-E/f:N/f:n/text()="JLON"]' .
                      '[./f:do-construct/f:do-stmt/f:do-V/f:named-E/f:N/f:n/text ()="JLEV"]/f:do-stmt', $doc);


    for my $do_jlon (@do_jlon)
      {
        my @do_jlev = &f ('../f:do-construct/f:do-stmt[./f:do-V/f:named-E/f:N/f:n/text()="JLEV"]', $do_jlon);
        my $do_jlev = $do_jlev[0]->cloneNode (1);
        for my $do_jlev (@do_jlev)
          {
            $do_jlev->replaceNode ($do_jlon->cloneNode (1));
          }
        $do_jlon->replaceNode ($do_jlev);
      }


  }


'FileHandle'->new (">$F90.new")->print ($doc->textContent);
