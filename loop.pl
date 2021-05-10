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

sub handle_do_stmt
{
 return ();
}

sub handle_end_do_stmt
{
 return ();
}

sub expr
{
  my $e = shift;
  my @e = &f ('descendant-or-self::f:named-E', $e);
  my %seen;
  @e = grep 
    { 
      my $e = $_;
      ($e->textContent !~ m/^(?:JLON|JLEV)$/o) &&
      do
        {
          my ($n) = &f ('./f:N/f:n/text ()', $e, 1);
          $n !~ m/^(?:MIN|MAX)$/o
        } 
      && (! $seen{$e->textContent}++)
    } @e;
  return @e;
}

sub handle_a_stmt
{
  shift; my ($stmt, $ctx) = @_;
  my ($e1) = &f ('./f:E-1/*', $stmt);
  my ($e2) = &f ('./f:E-2/*', $stmt);
  return ($stmt, $e1, &expr ($e2), @{ $ctx->{cond} });
}

sub handle_if_stmt
{
  shift; my ($stmt, $ctx) = @_;
  my ($e) = &f ('./f:condition-E/*', $stmt);
  $stmt = shift (@{$ctx->{stmt}});
  ($stmt) = &f ('./*', $stmt);
  return ($stmt, 'main'->handle ($stmt, $ctx), &expr ($e));
}

sub handle_if_then_stmt
{
  shift; my ($stmt, $ctx) = @_;
  my ($e) = &f ('./f:condition-E/*', $stmt);
  push @{ $ctx->{cond} }, &expr ($e);
  $ctx->{ncond}++;
  return ();
}

sub handle_else_if_stmt
{
  shift; my ($stmt, $ctx) = @_;
  my ($e) = &f ('./f:condition-E/*', $stmt);
  push @{ $ctx->{cond} }, &expr ($e);
  $ctx->{ncond}++;
  return ();
}

sub handle_else_stmt
{
  shift; my ($stmt, $ctx) = @_;
  return ();
}

sub handle_end_if_stmt
{
  shift; my ($stmt, $ctx) = @_;
  pop (@{ $ctx->{cond} }) for (1 ..  $ctx->{ncond});
  $ctx->{ncond} = 0;
  return ();
}

sub handle
{
  shift; my ($stmt, $ctx) = @_;
  (my $method = 'handle_' . $stmt->nodeName) =~ s/-/_/go;
  die $stmt->toString unless ('main'->can ($method));
  my @res = 'main'->$method ($stmt, $ctx);
  return @res;
}


my ($F90) = @ARGV;

my $doc = &Fxtran::fxtran (location => $F90);

my @do = &f ('.//f:do-construct[./f:do-stmt/f:do-V/f:named-E/f:N/f:n/text ()="JLEV"]', $doc);

for my $do (@do)
  {
    print "\n";
    print "-" x 80, "\n";
    print $do->textContent, "\n";
    print "-" x 80, "\n";


    my ($step) = &f ('./f:do-stmt/f:step', $do);

    $step &&= $step->textContent;
    $step ||= "+1";

    $step && print "step=$step\n";

    my @stmt = &f ('.//f:' . &Fxtran::xpath_by_type ('stmt'), $do);
    my $ctx = {stmt => \@stmt, cond => [], ncond => 0};

    while (my $stmt = shift (@stmt))
      {

        my @res = 'main'->handle ($stmt, $ctx);


        @res && print &Dumper ([map { $_->textContent } @res]);


      }

    print "-" x 80, "\n";

  }
