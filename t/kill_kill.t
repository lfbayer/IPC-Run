#!/usr/bin/perl -w

=head1 NAME

kill_kill.t - Test suite IPC::Run->kill_kill

=cut

use strict ;

use Test ;

use IPC::Run qw( start ) ;

my @quiter = ( $^X, '-e', 'sleep while 1' ) ;
my @zombie00 = ( $^X, '-e', '$SIG{TERM}=sub{};$|=1;print "Ok\n";sleep while 1');

my @tests = (
sub {
   my $h = start \@quiter ;
   my $needed_kill = $h->kill_kill( grace => 2 ) ;
   ok ! $needed_kill ;
},

sub {
   my $out ;
   my $h = start \@zombie00, \undef, \$out ;
   pump $h until $out =~ /Ok/ ;
   my $needed_kill = $h->kill_kill( grace => 1 ) ;
   ok $needed_kill ;
},

## not testing coredumps; some systems don't provide them.

) ;

plan tests => scalar @tests ;

$_->() for ( @tests ) ;
