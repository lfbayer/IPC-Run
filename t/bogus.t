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
) ;

plan tests => scalar @tests ;

$_->() for ( @tests ) ;
