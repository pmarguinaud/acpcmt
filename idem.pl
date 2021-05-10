#!/usr/bin/perl -w
#
use strict;
use FileHandle;
use Data::Dumper;

local $SIG{__DIE__} = sub
{
  use Devel::StackTrace;
  my $trace = 'Devel::StackTrace'->new ();
  print $trace->as_string; # like carp
};

use FindBin qw ($Bin);
use lib $Bin;
use Fxtran;

my $F90 = shift;

unlink ("$F90.xml");
my $doc = &Fxtran::fxtran (location => $F90);

'FileHandle'->new (">$F90.idem")->print ($doc->textContent);
