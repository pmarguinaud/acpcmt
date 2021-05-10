#!/usr/bin/perl -w
#
use strict;
use FileHandle;
use Data::Dumper;

use FindBin qw ($Bin);
use lib $Bin;
use Fxtran;

my ($F90, $var) = @ARGV;

my $doc = &Fxtran::fxtran (location => $F90);




