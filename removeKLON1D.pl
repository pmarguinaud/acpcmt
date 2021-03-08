#!/home/ms/fr/sor/install/perl-5.32.1/bin/perl -w
#
use strict;
use FileHandle;
use Data::Dumper;

use FindBin qw ($Bin);
use lib $Bin;
use Fxtran;


my $F90 = shift;

my $doc = &Fxtran::fxtran (location => $F90);

my $code = do { my $fh = 'FileHandle'->new ("<$F90"); local $/ = undef; <$fh> };

# temp (REAL(KIND=JPRB), ZQV, (KLON))

my @v = ($code =~ m/temp \(((?:REAL|INTEGER)\(KIND=\w+\)), (\w+), \(KLON\)\)/go);

my %v = reverse (@v);


my @en_decl  = &f ('.//f:EN-decl[./f:array-spec/f:shape-spec-LT/f:shape-spec/f:upper-bound[.//text ()="KLON"]][count (.//f:shape-spec)=1]/f:EN-N/f:N/f:n/text ()', $doc);
my @en_decl_n = map { $_->textContent } @en_decl;

for my $v (keys (%v), @en_decl_n)
  {
    my @r = &f ('.//f:named-E[./f:N/f:n/text ()="?"]/f:R-LT', $v, $doc);
    for (@r)
      {
        $_->unbindNode ();
      }
  }

my @as  = &f ('.//f:EN-decl/f:array-spec[./f:shape-spec-LT/f:shape-spec/f:upper-bound[.//text ()="KLON"]][count (.//f:shape-spec)=1]', $doc);

for (@as)
  {
    $_->unbindNode ();
  }


my @do = &f ('.//f:do-construct[./f:do-stmt/f:do-V/f:named-E/f:N/f:n/text ()="JLON"]', $doc);

$do[0]->parentNode->insertBefore (&t ("JLON = KIDIA\n"), $do[0]);

for my $do (@do)
  {
    $do->firstChild->unbindNode ();
    $do->lastChild->unbindNode ();
  }


my $va = join ('|', keys (%v));
$code = $doc->textContent;
$code =~ s/temp \(((?:REAL|INTEGER)\(KIND=\w+\)), (\w+), \(KLON\)\)/$1 :: $2/goms;
$code =~ s/alloc \(\b(?:$va)\b\)[ ]*\n//goms;

'FileHandle'->new (">$F90.new")->print ($code);




