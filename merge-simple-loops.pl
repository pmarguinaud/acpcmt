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

    my $xpath = './f:do-construct[./f:do-stmt/f:do-V/f:named-E/f:N/f:n/text()="JBLK"]' .
                '[.//f:do-construct[./f:do-stmt/f:do-V/f:named-E/f:N/f:n/text()="JLEV"]' .
                '[./f:do-construct[./f:do-stmt/f:do-V/f:named-E/f:N/f:n/text()="JLON"]' .
                '[count (.//f:' . &Fxtran::xpath_by_type ('stmt') . ')=3]]]';

    my @do = &f (".//$xpath", $pu);

    for my $do (@do)
      {
        next unless ($do->parentNode);  # Loop already merged

        my ($stmt) = &f ('./f:do-construct/f:do-construct/f:' . &Fxtran::xpath_by_type ('stmt') . '[2]', $do);

        next unless ($stmt->nodeName eq 'a-stmt');
        next unless (&f ('./f:E-2/f:literal-E', $stmt));
         


        my @s = &f ('following-sibling::node ()', $do);
        print $do->textContent, "\n";

        
        my @mrg = ($do);
        my @del = ([]);

        for my $s (@s)
          {
            if ($s->nodeName =~ m/^(?:C|#text)$/o)
              {
                push @{ $del[-1] }, $s;
                next;
              }
            if (($s->nodeName eq 'do-construct') && grep { $s->isSameNode ($_) } @do)
              {
                push @mrg, $s;
                push @del, [];
                next;
              }
            pop @del;
            last;
          }

        print &Dumper ([map { $_->textContent } @mrg]);
        print &Dumper ([map { $_->textContent } map { @$_ } @del]);
        print '-' x 80, "\n";

        

      }



  }


'FileHandle'->new (">$F90.new")->print ($doc->textContent);
