#!/usr/bin/perl -w

=head1 NAME

bogus.t - test bogus file cases.

=cut

use strict ;

use Test ;

use IPC::Run qw( start ) ;
use UNIVERSAL qw( isa ) ;

my $r ;

my @tests = (
sub {
   ## Older Test.pm's don't grok qr// in $expected.
   my $expected = 'file not found' ;
   eval { start ["./bogus_really_bogus"] } ;
   my $got = $@ =~ $expected ? $expected : $@ || "" ;
   ok $got, $expected, "starting ./bogus_really_bogus" ;
},

sub {
   ## Older Test.pm's don't grok qr// in $expected.
   my $expected = 'exec failed' ;
   my $h = eval {
      start [$^X, "-e", 1], _simulate_exec_failure => 1 ;
   } ;
   my $got = $@ =~ $expected ? $expected : $@ || "" ;
   ok $got, $expected, "starting $^X with simulated_exec_failure => 1" ;
},

) ;

plan tests => scalar @tests ;

$_->() for ( @tests ) ;
