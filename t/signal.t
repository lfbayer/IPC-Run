#!/usr/bin/perl -w

=head1 NAME

signal.t - Test suite IPC::Run->signal

=cut

use strict ;

use Test ;

use IPC::Run qw( start ) ;

my @receiver = (
   $^X,
   '-e',
   <<'END_RECEIVER',
      sub s{
	 print $_[0], "\n";
      }
      $SIG{$_}=\&s for (qw(USR1 USR2));
      $| = 1 ;
      print "Ok\n";
      sleep 1 ;
      sleep 1 ;
      sleep 1 ;
      sleep 1 ;
      sleep 1 ;
END_RECEIVER
) ;

# $IPC::Run::debug=10 ;

my $h ;
my $out ;

my @tests = (
sub {
   $h = start \@receiver, \undef, \$out ;
   pump $h until $out =~ /Ok/ ;
   ok 1 ;
},
sub {
   $out = "" ;
   $h->signal( "USR2" ) ;
   pump $h ;
   $h->signal( "USR1" ) ;
   pump $h ;
   $h->signal( "USR2" ) ;
   pump $h ;
   $h->signal( "USR1" ) ;
   pump $h ;
   ok $out, "USR2\nUSR1\nUSR2\nUSR1\n" ;
},

sub {
   $h->signal( "QUIT" ) ;
   finish $h ;
   ok( 1 ) ;
},

) ;

plan tests => scalar @tests ;

$_->() for ( @tests ) ;
