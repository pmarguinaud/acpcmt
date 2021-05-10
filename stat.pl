#!/usr/bin/perl -w
#
use strict;
use FileHandle;
use Data::Dumper;

use XML::LibXML;

my $xml = shift;


my $doc = 'XML::LibXML'->load_xml (location => $xml);

my $xpc = 'XML::LibXML::XPathContext'->new ();

my @n = $xpc->findnodes ('.//node ()', $doc);
my @e = $xpc->findnodes ('.//*', $doc);

print scalar (@n), "\n";
print scalar (@e), "\n";
