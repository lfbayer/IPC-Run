#!/usr/bin/perl -w

=head1 NAME

pty.t - Test suite for IPC::Run's pty (psuedo-terminal) support

=cut

use strict ;

use Test ;

use IPC::Run qw( start pump finish ) ;
use UNIVERSAL qw( isa ) ;

my $echoer_script = <<TOHERE ;
\$| = 1 ;
\$s = select STDOUT ; \$| = 1 ; select \$s ;
while (<>) {
   print ;
   print STDERR uc \$_ ;
   last if /quit/ ;
}
TOHERE

##
## $^X is the path to the perl binary.  This is used run all the subprocesses.
##
my @echoer = ( $^X, '-e', $echoer_script ) ;

my $in ;
my $out ;
my $err;

my $h ;
my $r ;

my $fd_map ;

my $text = "hello world\n" ;

sub map_fds() { &IPC::Run::_map_fds }

# $IPC::Run::debug = 2 ;

## TODO: test lots of mixtures of pty's and pipes & files.  Use run().

my @tests = (
##
## stdin only
##
sub {
   $out = 'REPLACE ME' ;
   $? = 99 ;
   $fd_map = map_fds ;
   $h = start \@echoer, '<pty<', \$in, '>', \$out, '2>', \$err ;

   $in  = "hello\n" ;
   $? = 0 ;
   pump $h until $out =~ /hello/ ;
   ok( $out, "hello\n" ) ;
},
sub { ok( $err =~ /^HELLO\n(?!\n)$/ ) },
sub { ok( $in, '' ) },

sub {
   $in  = "world\n" ;
   $? = 0 ;
   pump $h until $out =~ /world/ ;
  ok( $out, "hello\nworld\n" ) ;
},
sub { ok( $err =~ /^HELLO\nWORLD\n(?!\n)$/ ) },
sub { ok( $in, '' ) },

sub {
   $in = "quit\n" ;
   ok( $h->finish ) ;
},
sub { ok( ! $? ) },
sub { ok( map_fds, $fd_map ) },

##
## stdout, stderr
##
sub {
   $out = 'REPLACE ME' ;
   $? = 99 ;
   $fd_map = map_fds ;
   $h = start \@echoer, \$in, '>pty>', \$out ;
   $in  = "hello\n" ;
   $? = 0 ;
   pump $h until $out =~ /hello/ ;
   ## We assume that the slave's write()s are atomic
   ok( $out =~ /^(?:hello\r?\n){2}(?!\n)$/i ) ;
},
sub { ok( $in, '' ) },

sub {
   $in  = "world\n" ;
   $? = 0 ;
   pump $h until $out =~ /world/ ;
   ok( $out =~ /^(?:hello\r?\n){2}(?:world\r?\n){2}(?!\n)$/i ) ;
},
sub { ok( $in, '' ) },

sub {
   $in = "quit\n" ;
   ok( $h->finish ) ;
},
sub { ok( ! $? ) },
sub { ok( map_fds, $fd_map ) },

##
## stdout only
##
sub {
   $out = 'REPLACE ME' ;
   $? = 99 ;
   $fd_map = map_fds ;
   $h = start \@echoer, \$in, '>pty>', \$out, '2>', \$err ;
   $in  = "hello\n" ;
   $? = 0 ;
   pump $h until $out =~ /hello/ ;
   ok( $out =~ /^hello\r?\n(?!\n)$/ ) ;
},
sub { ok( $err =~ /^HELLO\n(?!\n)$/ ) },
sub { ok( $in, '' ) },

sub {
   $in  = "world\n" ;
   $? = 0 ;
   pump $h until $out =~ /world/ ;
   ok( $out =~ /^hello\r?\nworld\r?\n(?!\n)$/ ) ;
},
sub { ok( $err =~ /^HELLO\nWORLD\n(?!\n)$/ ) },
sub { ok( $in, '' ) },

sub {
   $in = "quit\n" ;
   ok( $h->finish ) ;
},
sub { ok( ! $? ) },
sub { ok( map_fds, $fd_map ) },

##
## stdin, stdout, stderr
##
sub {
   $out = 'REPLACE ME' ;
   $? = 99 ;
   $fd_map = map_fds ;
   $h = start \@echoer, '<pty<', \$in, '>pty>', \$out ;
   $in  = "hello\n" ;
   $? = 0 ;
   pump $h until $out =~ /hello.*hello.*hello/is ;
   ## We assume that the slave's write()s are atomic
   ok( $out =~ /^(?:hello\r?\n){3}(?!\n)$/i ) ;
},
sub { ok( $in, '' ) },

sub {
   $in  = "world\n" ;
   $? = 0 ;
   pump $h until $out =~ /world.*world.*world/is ;
   ok( $out =~ /^(?:hello\r?\n){3}(?:world\r?\n){3}(?!\n)$/i ) ;
},
sub { ok( $in, '' ) },

sub {
   $in = "quit\n" ;
   ok( $h->finish ) ;
},
sub { ok( ! $? ) },
sub { ok( map_fds, $fd_map ) },
) ;

plan tests => scalar @tests ;

$_->() for ( @tests ) ;
