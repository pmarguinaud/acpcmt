#!/usr/bin/perl -w
#
use strict;
use FileHandle;
use Data::Dumper;
use Storable;

use FindBin qw ($Bin);
use lib $Bin;
use Fxtran;
use Frame;

sub getShapeSpecList
{
  my ($var, $doc, $rw, $cr) = @_;

  my @en_decl = &f ('//f:EN-decl[./f:EN-N/f:N/f:n/text ()="' . $var . '"]', $doc);

  my $sslt;

  # Update dimensions : look for existing array spec

  for my $en_decl (@en_decl)
    {
      ($sslt) = &f ('./f:array-spec/f:shape-spec-LT', $en_decl);
      if ($sslt)
        {
          last;
        }
    }

  # No dimensions: add array spec

  if ((! $sslt) && ($cr))
    {
      for my $en_decl (@en_decl)
        {
          my $as = $en_decl->appendChild (&n ("<array-spec/>"));
          $as->appendChild (&t ("("));
          $sslt = $as->appendChild (&n ("<shape-spec-LT/>"));
          $as->appendChild (&t (")"));
          last;
        }
    }

  return $sslt;
}


sub escape
{
  my $t = &t ($_[0]);
  return $t->toString;
}

sub attr
{
  my ($o, $n) = @_;
  ($o) = &f ("./f:$n", $o);
  return $o;
}

sub expandIfThenElse
{
  my $do = shift;

  my ($if) = &f ('.//f:if-construct', $do);
 
  return ($do->cloneNode (1)) unless ($if);

  my @block = &f ('./f:if-block', $if);
  my @do = map { $do->cloneNode (1) } @block;

  for my $i (0 .. $#do)
    {
      ($if) = &f ('.//f:if-construct', $do[$i]);
      @block = &f ('./f:if-block', $if);

      my ($cr) = &f ('preceding::text ()[contains (., "' . "\n" . '")]', $block[$i]);
      (my $sp = $cr->textContent) =~ s/^.*\n//o;

      my ($cond) = &f ('./*/f:condition-E/*', $block[$i]);
      my $C = &n ('<C/>');
      $if->replaceNode ($C);
      $C->parentNode->insertBefore (&n ('<C>! expandIfThenElse ' . ($cond ? &escape ($cond->textContent) : 'NONE') . " ($i)</C>"), $C);
      $C->parentNode->insertBefore (&t ("\n"), $C);
      $C->parentNode->insertBefore (&t ($sp), $C);

      my @n = &f ('./node ()', $block[$i]);
      shift (@n);  # Remove IF or ELSEIF statements
      pop (@n) if ($n[-1]->nodeName eq 'end-if-stmt');
      for my $n (@n)
        {
          $C->parentNode->insertBefore ($n, $C);
        }
      $C->unbindNode ();
    }

 
  return map { &expandIfThenElse ($_) } @do;
}



sub loopDepend
{
  my ($do, $pu, $fh) = @_;

  my $stmt = $do->firstChild;

  my @dep;

  my @assign = &f ('.//f:a-stmt', $do);

  my %n1;
  my %n2;

  for my $assign (@assign)
    {
      my ($e1) = &f ('./f:E-1/*', $assign);

      my @ar1 = map { $_->textContent } 
                &f ('./f:R-LT/f:parens-R/f:element-LT/f:element/*', $e1);

      my $n1 = &attr ($e1, 'N')->textContent;
      $n1{$n1}++;

      my @e2 = &f ('./f:E-2//f:named-E[not (ancestor::f:R-LT)]', $assign);

      my @d2;

      for my $e2 (@e2)
        {
          my @ar2 = map { $_->textContent } 
                    &f ('./f:R-LT/f:parens-R/f:element-LT/f:element/*', $e2);

          my $n2 = &attr ($e2, 'N')->textContent;

          my $s2 = \@ar2;

          if ((scalar (@ar2) > 1) && (! grep { $_ =~ m/JLEV/o } @ar2))
            {
              my $as2 = &getShapeSpecList ($n2, $pu);
              my @as2 = map { $_->textContent } &f ('./f:shape-spec', $as2);
              if (grep { $_ =~ m/KLEV/o } @as2)
                {
                  $n2{$n2} = 1;
                }
            }

          push @d2, [$n2, \@ar2];

        }

      push @dep, [$n1, \@ar1, \@d2, $assign->textContent];

    }

  
  if (my @n2 = grep { $n1{$_} } keys (%n2))
    {
      # Here we handle this special case:
      # An array with KLEV dimension is modified and used with a fixed JLEV (=KLEV for instance)
      # index; in this case iterations are not independent

=pod

SUBROUTINE TITI

INTEGER :: KN (KLON, KLEV)

DO JLEV = 1, KLEV
  DO JLON = 1, KLON
    KN (JLON, JLEV) = KN (JLON, KLEV) + 1
  ENDDO
ENDDO

END SUBROUTINE TITI

=cut

      return 1;
    }



# $fh->print (&Dumper (\@dep));


  for my $dep1 (@dep)
    {
      my $spc = '            ';
      $fh->printf ("%-20s%s| ", $dep1->[0], $spc);
      for my $d (@{$dep1->[1]})
        {
          $fh->printf ("%-8s", $d);
        }
      $fh->print ("\n");
      for my $dep2 (@{$dep1->[2]})
        {
          $fh->printf ("%s%-20s| ", $spc, $dep2->[0]);
          for my $d (@{$dep2->[1]})
            {
              $fh->printf ("%-8s", $d);
            }
          $fh->print ("\n");
        }
      $fh->printf ("\n");
    }

  # Two iterations forward/backward  

  my @way = qw (m1p0p1 p1p0m1);

  my %ind = (
              m1p0p1 => [-1, +0, +1], 
              p1p0m1 => [+1, +0, -1],
            );

  my $addJLEV = sub 
  { 
    my $x = shift;
    my @ss = @_;
    for my $ss (@ss)
      {
        if ($ss =~ m/^JLEV([+-]\d)?$/o)
          {
            my $i = ($1 || 0) + $x;
            $ss = 'JLEV' . ($i ? sprintf ('%+d', $i) : '');
          }
      }
    return @ss;
  };

  my $arrayRef = sub
  {
    return '' unless (@_);
    return '(' . join (',', @_) . ')';
  };

  my %dep;

  for my $way (@way)
    {  
$fh->print (&frame ($way, 80));
      for my $ind (@{ $ind{$way} })
        {
$fh->print (&frame ($ind, 80));
          for my $d1 (@dep)
            {
$fh->print ('-' x 80, "\n");
$fh->print ($d1->[3], "\n");
              my %h;
              my $k1 = $d1->[0] . $arrayRef->($addJLEV->($ind, @{ $d1->[1] }));
              for my $d2 (@{ $d1->[2] })
                {
                  my $k2 = $d2->[0] . $arrayRef->($addJLEV->($ind, @{ $d2->[1] }));
                  if (! exists $dep{$way}{$k2})
                    {
                      $h{$k2} = 1;
                    }
                  else
                    {
                      for (keys (%{ $dep{$way}{$k2} }))
                        {
                          $h{$_} = 1;
                        }
                    }
                }
              $dep{$way}{$k1} = \%h;
my @dd = sort keys (%{ $dep{$way}{$k1} });
$fh->printf ("%-20s : %s\n\n", $k1, "@dd");
            }
        }
    }

  
# $fh->print (&Dumper (\%dep));

  my $depend = 0;

  my $eq = sub { local $Storable::canonical = 1; &Storable::freeze ($_[0]) eq &Storable::freeze ($_[1]) };
  for my $d1 (@dep)
    {
      my $k1 = $d1->[0] . $arrayRef->(@{$d1->[1]});
      next unless ($k1 =~ m/\(/o); # Skip non array variables
      next if ($eq->(map { $dep{$_}{$k1} } @way));
      $fh->printf ("%-20s\n  %s : %s\n  %s : %s\n", $k1, map { ($_, join (' ', sort keys (%{ $dep{$_}{$k1}}))) } @way);
      $depend++;
    }
 
  return $depend;


}

sub replaceNodeByChildNodes
{
  my $n = shift;
  my @c = $n->childNodes ();

  for my $c (@c)
    {
      $n->parentNode->insertBefore ($c, $n);
    }

  $n->unbindNode ();
}

sub muteExprFunction
{
  my $e = shift;

  $e->setNodeName ('function-E');
  my $rlt = &attr ($e, 'R-LT');
  my $r = $rlt->firstChild;
  $rlt->replaceNode ($r);
  &replaceNodeByChildNodes ($r);
  &attr ($e, 'element-LT')->setNodeName ('arg-spec');
  for (&f ('./f:arg-spec/f:element', $e))
    {
      $_->setNodeName ('arg');
    }
}

sub resolveFunctions
{
  my $doc = shift;

  my @e = &f ('.//f:named-E[./f:N/f:n/text ()[' . join (' or ', map { ".=\"$_\"" } @_) . ']]', $doc);

  for my $e (@e)
    {
      &muteExprFunction ($e);
    }
}

sub resolveIntrinsics
{
  my $doc = shift;
  my @intrinsics = qw (MAX ABS SIGN MIN NINT ISIGN SIN SQRT LOG COS TAN REAL EXP FLOAT);
  &resolveFunctions ($doc, @intrinsics);
}

sub grokInlineFunctions
{
  my $f = shift;
  my $code = do { my $fh = 'FileHandle'->new ("<$f"); local $/ = undef; <$fh> };
  grep { $_ ne 'REAL' } ($code =~ m/\n\s*(\w+)\s*\(/goms);
}

my $F90 = shift;

my $doc = &Fxtran::fxtran (location => $F90);

&resolveIntrinsics ($doc);

&resolveFunctions ($doc, &grokInlineFunctions ('fctdoi.func.h'));
&resolveFunctions ($doc, &grokInlineFunctions ('fcttrm.func.h'));

my @do = &f ('.//f:do-construct[./f:do-stmt/f:do-V/f:named-E/f:N/f:n/text ()="JLEV"]', $doc);

my $i = 0;

for my $do (@do)
  {
    my @do = &expandIfThenElse ($do);

    my $j = 0;
    'FileHandle'->new ('>' . sprintf ('loop.%4.4d.F90', $i))->print ($do->textContent);
    for my $do (@do)
      {
        my $f = sprintf ('loop.%4.4d.%4.4d.F90', $i, $j);
        my $fh = 'FileHandle'->new (">$f");
        $fh->print ($do->textContent . "\n\n");
        my $dep = &loopDepend ($do, $doc, $fh);
        print $f, " ", $dep, "\n";
        $fh->print ("\n\n$dep\n");
      
        $j++;
      }

    $i++;
  }
