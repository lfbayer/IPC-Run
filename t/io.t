#!/usr/bin/perl -w

=head1 NAME

io.t - Test suite excercising IPC::Run::IO with IPC::Run::run.

=cut

use strict ;

use Test ;

use IPC::Run qw( :filters run io ) ;
use UNIVERSAL qw( isa ) ;

my $text    = "Hello World\n" ;

my $emitter_script = qq{print '$text' ; print STDERR uc( '$text' )} ;
##
## $^X is the path to the perl binary.  This is used run all the subprocesses.
##
my @perl    = ( $^X ) ;
my @emitter = ( @perl, '-e', $emitter_script ) ;

my $recv ;
my $send ;

my $in_file  = 'io.t.in' ;
my $out_file = 'io.t.out' ;
my $err_file = 'io.t.err' ;

my $io ;
my $r ;

my $fd_map ;
sub map_fds() { &IPC::Run::_map_fds }

# $IPC::Run::debug = 2 ;

## TODO: Test filters, etc.

sub slurp($) {
   my ( $f ) = @_ ;
   open( S, "<$f" ) or return "$! '$f'" ;
   my $r = join( '', <S> ) ;
   close S or warn "$! closing '$f'";
   return $r ;
}


sub spit($$) {
   my ( $f, $s ) = @_ ;
   open( S, ">$f" ) or die "$! '$f'" ;
   print S $s       or die "$! '$f'" ;
   close S          or die "$! '$f'" ;
}

sub wipe($) {
   my ( $f ) = @_ ;
   unlink $f or warn "$! unlinking '$f'" if -f $f ;
}



my @tests = (
##
## Parsing
##
sub {
   $io = io( 'foo', '<', \$send ) ;
   ok( ref $io, 'IPC::Run::IO' ) ;
},

sub { ok( io( 'foo', '<',  \$send  )->mode, 'w'  ) },
sub { ok( io( 'foo', '<<', \$send  )->mode, 'wa' ) },
sub { ok( io( 'foo', '>',  \$recv  )->mode, 'r'  ) },
sub { ok( io( 'foo', '>>', \$recv  )->mode, 'ra' ) },

##
## Input from a file
##
sub {
   spit $in_file, $text ;
   $recv = 'REPLACE ME' ;
   $fd_map = map_fds ;
   $r = run io( $in_file, '>', \$recv ), debug => 10 ;
   wipe $in_file ;
   ok( $r ) ;
},
sub { ok( ! $? ) },
sub { ok( map_fds, $fd_map ) },

sub { ok( $recv, $text ) },

##
## Output to a file
##
sub {
   wipe $out_file ;
   $send = $text ;
   $fd_map = map_fds ;
   $r = run io( $out_file, '<', \$send ) ;
   $recv = slurp $out_file ;
   wipe $out_file ;
   ok( $r ) ;
},
sub { ok( ! $? ) },
sub { ok( map_fds, $fd_map ) },

sub { ok( $send, $text ) },
sub { ok( $recv, $text ) },
) ;

plan tests => scalar @tests ;

$_->() for ( @tests ) ;
