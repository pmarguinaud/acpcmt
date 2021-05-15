#!/usr/bin/perl -w
#
use strict;
use FileHandle;
use Data::Dumper;

use FindBin qw ($Bin);
use lib $Bin;
use Fxtran;

sub getShapeSpecList
{
  my ($var, $doc, %opts) = @_; 

  my $cr = $opts{create};

  my @en_decl = &f ('.//f:EN-decl[./f:EN-N/f:N/f:n/text ()="' . $var . '"]', $doc);

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

        my %a = map { ($_->textContent, 1) } @a;
        @a = sort keys (%a);

        my $sp = $do->previousSibling;
        ($sp = $sp->textContent) =~ s/^\s*\n//o;
        $do->parentNode->insertBefore (&n ('<C>!$acc parallel loop gang vector collapse (2) vector_length (KLON) '
                                         . (@p ? 'private (' . join (', ', @p) . ') ' : '')
                                         . 'default (none)'
                                         . '</C>'), $do);
        $do->parentNode->insertBefore (&t ("\n"), $do);
        $do->parentNode->insertBefore (&t ($sp), $do);
      }


  }

for my $pu (@pu)
  {
    my @arg = map { $_->textContent } &f ('.//f:dummy-arg-LT/f:arg-N/f:N/f:n/text ()', $pu);
    my %arg = map { ($_, 1) } @arg;

    my @aa;

    for my $arg (@arg)
      {
        my $as = &getShapeSpecList ($arg, $pu);
        push @aa, $arg if ($as);
      }

    @aa = sort @aa;

    # Local arrays 

    my @la = sort grep { ! $arg{$_} } 
             map { $_->textContent } &f ('.//f:EN-decl[./f:array-spec]/f:EN-N/f:N/f:n/text ()', $pu);
    
    my @stmt = &f ('.//f:T-decl-stmt|.//f:include', $pu);
    my $stmt = $stmt[-1];

    my ($cr) = &f ('following::text ()[contains (., "' . "\n" . '")]', $stmt);
    (my $sp = $cr->textContent) =~ s/\n\s*//o;


    my $nacc = 0;

    while (@la)
      {
        $cr->parentNode->insertAfter (&t ("\n"), $cr);
        $cr->parentNode->insertAfter (&n ('<C>!$acc data '
                                        . 'create (' . join (', ', splice (@la, 0, 10)) . ')'
                                        . '</C>'),  $cr);
        $cr->parentNode->insertAfter (&t ($sp), $cr);
        $nacc++;
      }
   
    while (@aa)
      {
        $cr->parentNode->insertAfter (&t ("\n"), $cr);
        $cr->parentNode->insertAfter (&n ('<C>!$acc data '
                                        . 'present (' . join (', ', splice (@aa, 0, 10)) . ')'
                                        . '</C>'),  $cr);
        $cr->parentNode->insertAfter (&t ($sp), $cr);
        $nacc++;
      }
   
    while ($nacc)
      {
        $pu->insertBefore (&t ($sp), $pu->lastChild);
        $pu->insertBefore (&n ('<C>!$acc end data</C>'), $pu->lastChild);
        $pu->insertBefore (&t ("\n"), $pu->lastChild);
        $nacc--;
      }
    

  }


'FileHandle'->new (">$F90.new")->print ($doc->textContent);
