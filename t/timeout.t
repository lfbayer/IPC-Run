#!/usr/bin/perl -w

=head1 NAME

timeout.t - Test suite for IPC::Run timeouts

=cut

## Separate from run.t so run.t is not too slow.

use strict ;

use Test ;

use IPC::Run qw( harness ) ;
use UNIVERSAL qw( isa ) ;

my $h ;
my $in ;
my $out ;
my $started ;

# $IPC::Run::debug = 2 ;

my @tests = (

sub {
   $h = harness( [ $^X ], \$in, \$out ) ;
   ok( isa( $h, 'IPC::Run' ) ) ;
},

sub { ok( ! defined $h->timeout ) },

sub { $h->timeout(  0   ) ;  ok( $h->timeout,   0 ) },
sub { $h->timeout(  0.1 ) ;  ok( $h->timeout      ) },
sub { $h->timeout(  1   ) ;  ok( $h->timeout,   1 ) },
sub { $h->timeout( 30   ) ;  ok( $h->timeout,  30 ) },
sub { $h->timeout( 30.1 ) ;  ok( $h->timeout > 30 ) },

sub { $h->timeout( "1:0" ) ;      ok( $h->timeout,    60 ) },
sub { $h->timeout( "1:0:0" ) ;    ok( $h->timeout,  3600 ) },
sub { $h->timeout( "1:1:1" ) ;    ok( $h->timeout,  3661 ) },
sub { $h->timeout( "1:1:1.1" ) ;  ok( $h->timeout > 3661 ) },

sub { $h->timeout( undef ) ; ok( ! defined $h->timeout ) },

sub {
   $h->timeout( 1 ) ;
   sleep 2 ;
   $started = time ;
   $h->start ;
   $in = '' ;
   eval { $h->pump };
   ok( $@ =~ /timed out/ ) ;
},

sub { ok( time - $started >= 1 ) },

sub {
   ok( $h->timeout, 1 ) ;
   $started = time ;
   $h->start ;
},
sub {
   $in = '' ;
   eval { $h->pump };
   ok( $@ =~ /timed out/ ) ;
},

sub { ok( time - $started >= 1 ) },

sub {
   $h = harness( [ $^X ], \$in, \$out, { timeout => 1 } ) ;
   $started = time ;
   $h->start ;
   $in = '' ;
   eval { $h->pump };
   ok( $@ =~ /timed out/ ) ;
},

sub { ok( time - $started >= 1 ) },

) ;



plan tests => scalar @tests ;

$_->() for ( @tests ) ;

