#!/usr/bin/perl -w
#
use strict;
use FileHandle;
use Data::Dumper;

use FindBin qw ($Bin);
use lib $Bin;
use Fxtran;

my ($f1, $f2) = @ARGV;

my $d1 = &Fxtran::fxtran (location => $f1);
my $d2 = &Fxtran::fxtran (location => $f2);

my ($s1) = &f ('.//f:subroutine-stmt', $d1);
my ($s2) = &f ('.//f:subroutine-stmt', $d2);

my ($n2) = &f ('./f:subroutine-N/f:N/f:n/text ()', $s2, 1);

my @da = &f ('./f:dummy-arg-LT/f:arg-N/f:N/f:n/text ()', $s2, 1);

my @call = &f ('.//f:call-stmt[./f:procedure-designator/f:named-E/f:N/f:n/text ()="?"]', $n2, $d1);

for my $call (@call)
  {
    my @aa = &f ('.//f:arg-spec/f:arg', $call);
    die $call->toString unless (@aa == @da);
    for my $aa (@aa)
      {
        # check we have a simple named expression without any reference 
        if (($aa->nodeName ne 'named-E') && (&f ('.//f:R-LT', $aa)))
          {
            die $aa->toString;
          }
      }
    @aa = map { $_->textContent } @aa;
    my %da2aa;
    for my $i (0 .. $#aa)
      {
        $da2aa{$da[$i]} = $aa[$i];
      }
    
    # Replace dummy arguments by actual arguments
    for my $da (@da)
      {
        my @n = &f ('.//f:named-E/f:N/f:n[text ()="?"]/text ()', $da, $d2);
        for (@n)
          {
            $_->replaceNode (&t ($da2aa{$da}));
          }
      }
  }

# Remove dummy arguments declaration

for my $da (@da)
  {
    my @en_decl = &f ('.//f:EN-decl/f:EN-N/f:N/f:n[text ()="?"]', $da, $d2);
    for my $en_decl (@en_decl)
      {
        my ($stmt) = &Fxtran::stmt ($en_decl);
        my ($cr) = &f ('following::text ()[contains (., "' . "\n" . '")]', $stmt);
        $stmt->unbindNode ();
        $cr->unbindNode ();
      }
  }

# Move use statements 

my @use1 = &f ('.//f:use-stmt', $d1);

my $stmt = $use1[-1];

$stmt ||= $s1;

my @use2 = &f ('.//f:use-stmt', $d2);

for my $use2 (reverse (@use2))
  {
    my ($cr) = &f ('following::text ()[contains (., "' . "\n" . '")]', $use2);
    $s1->parentNode ()->insertAfter ($use2, $stmt);
    $s1->parentNode ()->insertAfter (&t ("\n"), $stmt);
    $cr->unbindNode ();
  }

my @en_decl1 = &f ('.//f:EN-decl', $d1);
my @en_decl2 = &f ('.//f:EN-decl', $d2);

# Remove comments before declarations

if (@en_decl2)
  {
    my @C = &f ('preceding::f:C', $en_decl2[0]);
    for my $C (@C)
      {
        my ($cr) = &f ('following::text ()[contains (., "' . "\n" . '")]', $C);
        $C->unbindNode ();
        $cr->unbindNode ();
      }
  }

# Remove first and last statements

$s2->parentNode ()->lastChild ()->unbindNode ();
$s2->unbindNode ();

# Move declarations

for my $en_decl2 (@en_decl2)
  {
    my ($v2) = &f ('./f:EN-N/f:N/f:n/text ()', $en_decl2);

    my ($v1) = &f ('.//f:EN-decl[./f:EN-N/f:N/f:n[text ()="?"]]', $v2->textContent, $d1);

    # This variable exists in first subroutine
    if ($v1)
      {
        $v2->replaceNode (&t ("$v2\_$n2"));
        my @e2 = &f ('.//f:named-E/f:N/f:n[text ()="?"]/text ()', $v2, $d2);
        for my $e2 (@e2) 
          {
            $e2->replaceNode (&t ("$v2\_$n2"));
          }
      }
  }

my @decl_stmt1 = &f ('.//f:' . &Fxtran::xpath_by_type ('stmt') . '[.//f:EN-decl]', $d1);

my $decl_stmt1 = $decl_stmt1[-1];

for my $decl_stmt2 (&f ('.//f:' . &Fxtran::xpath_by_type ('stmt') . '[.//f:EN-decl]', $d2))
  {
    $decl_stmt1->parentNode->insertAfter ($decl_stmt2, $decl_stmt1);
    $decl_stmt1->parentNode->insertAfter (&t ("\n"), $decl_stmt1);
  }

for my $call (@call)
  {
    my $d11 = $d1->cloneNode (1);
    
  }



print $d2->textContent ();


