#!/home/ms/fr/sor/install/perl-5.32.1/bin/perl -w
#
use strict;
use GraphViz;
use FileHandle;
use Data::Dumper;

use FindBin qw ($Bin);
use lib $Bin;
use Fxtran;




my %g;
my %L;


for my $F90 (@ARGV)
  {

    my $doc = &Fxtran::fxtran (location => $F90);
    
    my @pu = &f ('./f:object/f:file/f:program-unit', $doc);
    
    my $pu = $pu[0];
    
    my $stmt = $pu->firstChild;
    
    my ($name) = &f ('./f:subroutine-N/f:N/f:n/text ()', $stmt);
    $name = $name->textContent ();
    $name =~ s/^SIMPLE4_//o;
    
    my @call = &f ('.//f:call-stmt/f:procedure-designator//f:named-E/f:N/f:n/text ()', $pu);
    
    $g{$name} = [];

    my @code = do { my $fh = 'FileHandle'->new ("<$F90"); <$fh> };
    $L{$name} = scalar (@code);

    for (@call)
      {
        (my $n = $_->textContent) =~ s/^SIMPLE4_//o;
        push @{ $g{$name} }, $n;
      }
  }
 

my @root;

while (my ($k, $v) = each (%g))
  {
    my %seen;
    @$v = grep { ! ($seen{$_}++) } grep { $g{$_} } @$v;
  }

@root = keys (%g);

while (my ($k, $v) = each (%g))
  {
    my %v = map { ($_, 1) } @$v;
    @root = grep { ! $v{$_} } @root;
  }

print &Dumper (\@root);



my $root = join ('-', sort @root);


my $g = 'GraphViz'->new (rankdir => 'LR');

while (my ($k, $v) = each (%g))
  {
    $g->add_node ($k, label => "$k\n$L{$k}", shape => 'box');
    for (@$v)
      {
        $g->add_edge ($k, $_);
      }
  }

'FileHandle'->new (">$root.png")->print ($g->as_png);

