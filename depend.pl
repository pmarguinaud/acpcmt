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
  my $step = &attr ($stmt, 'step');
  $step = $step ? $step->textContent : '+1';
  $step = ($step eq '1') ? '+1' : $step;

  my @dep;

  my @assign = &f ('.//f:a-stmt', $do);

  for my $assign (@assign)
    {
      my ($e1) = &f ('./f:E-1/*', $assign);

      my @ar1 = grep { m/\bJLEV\b/o } map { $_->textContent } 
                &f ('./f:R-LT/f:parens-R/f:element-LT/f:element/*', $e1);
      die if (scalar (@ar1) > 1);

      my $n1 = &attr ($e1, 'N')->textContent;

      my $s1 = '';
      for my $ar1 (@ar1)
        {
          if ($ar1 =~ m/^JLEV([+-]1)?$/o)
            {
              $s1 = $1 || '+0';
            }
          else
            {
              die "$ar1\n";
            }
        }

      my @e2 = &f ('./f:E-2//f:named-E[not (ancestor::f:R-LT)]', $assign);

      my @d2;

      for my $e2 (@e2)
        {
          my @ar2 = grep { m/\bJLEV\b/o } map { $_->textContent } 
                    &f ('./f:R-LT/f:parens-R/f:element-LT/f:element/*', $e2);

          die if (scalar (@ar2) > 1);

          my $n2 = &attr ($e2, 'N')->textContent;

          my $s2 = '';
          for my $ar2 (@ar2)
            {
              if ($ar2 =~ m/^JLEV([+-]1)?$/o)
                {
                  $s2 = $1 || '+0';
                }
              else
                {
                  die "$ar2\n";
                }
            }

          push @d2, [$n2, $s2];

        }

      push @dep, [$n1, $s1, \@d2, $assign->textContent];

    }

=pod

! Result depends on order

DO JLEV = 1, KLEV
  DO JLON = 1, KLON
    IN (JLON) = KN (JLON, JLEV)
  ENDDO
ENDDO

=cut

  for my $dep (@dep)
    {
      my ($k1, $s1) = @{ $dep }[0, 1];
      next if (length ($s1));
      my $ss = &getShapeSpecList ($k1, $pu);
      my @ss = map { $_->textContent } &f ('./f:shape-spec', $ss);
      return 1 unless (grep { m/KLEV/o } @ss);
    }

# $fh->print (&Dumper (\@dep));

  for my $dep (@dep)
    {
      $fh->printf ("%-20s %2s : ", $dep->[0], $dep->[1]);
      for my $d (@{ $dep->[2] })
        {
          $fh->printf ("(%-20s %2s) ", $d->[0], $d->[1]);
        }
      $fh->printf ("\n");
    }

  # Two iterations forward/backward  

  my @way = qw (m1p0 p1p0);

  my %ind = (
              m1p0 => [-1, +0], 
              p1p0 => [+1, +0],
            );

  my %dep;

  for my $way (@way)
    {  
$fh->print (&frame ($way, 80));
      for my $ind (@{ $ind{$way} })
        {
$fh->print (&frame ($ind, 80));
          my $ss = sub { length ($_[0]) ? sprintf ("%+1.1d", $_[0] + $ind) : '' };
          for my $d1 (@dep)
            {
$fh->print ('-' x 80, "\n");
$fh->print ($d1->[3], "\n");
              my %h;
              my $k1 = $d1->[0] . $ss->($d1->[1]);
              for my $d2 (@{ $d1->[2] })
                {
                  my $k2 = $d2->[0] . $ss->($d2->[1]);
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
      my $k1 = $d1->[0] . $d1->[1];
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

    $i++;
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

  }
