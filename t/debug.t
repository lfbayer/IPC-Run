#!/usr/bin/perl -w

=head1 NAME

debug.t - Test suite for handling of debug sockets

=cut

use strict ;

use Test ;

use IPC::Run qw( :filters :filter_imp start run filter_tests ) ;
use UNIVERSAL qw( isa ) ;

##
## $^X is the path to the perl binary.  This is used run all the subprocesses.
##
my @cat = ( $^X, qw( -pe0 ) ) ;

my $in ;
my $out ;
my $err ;

my $h ;

my $fd_map ;

sub map_fds() { &IPC::Run::_map_fds }

open STDERR_SAVE, ">&STDERR" or die $! ;

my @tests = (
sub {
    $fd_map = map_fds() ;
    $in = "foo" ;
    open STDERR, "/dev/null" or die $! unless $IPC::Run::debug ;
    my $h = run debug => 10,
       \@cat, \$in,
       "|", \@cat,
       "|", \@cat, '>', \$out ;
   open STDERR, ">&STDERR_SAVE" or die $! ;
   ok( $out, "foo" ) ;
},
sub {
   ok( $fd_map, map_fds ) ;
},
sub {
    $fd_map = map_fds() ;
    $in = "foo" ;
    open STDERR, "/dev/null" or die $! unless $IPC::Run::debug ;
    my $h = run debug => 10,
       \@cat, \$in,
       "|", \@cat,
       "|", sub { while (<STDIN>) { print } }, '>', \$out ;
   open STDERR, ">&STDERR_SAVE" or die $! ;
   ok( $out, "foo" ) ;
},
sub {
   ok( $fd_map, map_fds ) ;
},
sub {
    $fd_map = map_fds() ;
    $in = "foo" ;
    open STDERR, "/dev/null" or die $! unless $IPC::Run::debug ;
    my $h = run debug => 10,
       sub { while (<STDIN>) { print } }, \$in,
       "|", sub { while (<STDIN>) { print } },
       "|", sub { while (<STDIN>) { print } }, '>', \$out ;
   open STDERR, ">&STDERR_SAVE" or die $! ;
   ok( $out, "foo" ) ;
},
sub {
   ok( $fd_map, map_fds ) ;
},
) ;

plan tests => scalar @tests ;

$_->() for ( @tests ) ;

close STDERR_SAVE or die $! ;
