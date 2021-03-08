#!/home/ms/fr/sor/install/perl-5.32.1/bin/perl -w
#
use strict;
use FileHandle;
use Data::Dumper;

use FindBin qw ($Bin);
use lib $Bin;
use Fxtran;


my $F90 = shift;
my @N = @ARGV;

my $doc = &Fxtran::fxtran (location => $F90);


for my $N (@N)
  {
    my @r = &f ('.//f:named-E[./f:N/f:n/text ()="?"]/f:R-LT', $N, $doc);
    
    for my $r (@r)
      {
        $r->unbindNode ();
      }

  }

'FileHandle'->new (">$F90.new")->print ($doc->textContent);




