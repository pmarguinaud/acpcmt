#!/usr/bin/perl -w
#

use strict;
use FileHandle;

(my $ref = do { my $fh = 'FileHandle'->new ("<.git/HEAD"); local $/ = undef; <$fh> }) =~ s/^ref:\s*//o;

my $vv = do { my $fh = 'FileHandle'->new ("<.git/$ref"); local $/ = undef; <$fh> };

print $vv;


