#!/usr/bin/perl -w

=head1 NAME

run.t - Test suite for IPC::Run::run, etc.

=cut

use strict ;

use Test ;

use IPC::Run qw( :filters :filter_imp start run filter_tests ) ;
use UNIVERSAL qw( isa ) ;

my $text    = "Hello World\n" ;

my $emitter_script = qq{print '$text' ; print STDERR uc( '$text' )} ;
##
## $^X is the path to the perl binary.  This is used run all the subprocesses.
##
my @perl    = ( $^X ) ;
my @emitter = ( @perl, '-e', $emitter_script ) ;

my $in ;
my $out ;
my $err ;

my $in_file  = 'run.t.in' ;
my $out_file = 'run.t.out' ;
my $err_file = 'run.t.err' ;

my $h ;

my $fd_map ;

sub map_fds() { &IPC::Run::_map_fds }

# $IPC::Run::debug = 2 ;

sub slurp($) {
   my ( $f ) = @_ ;
   open( S, "<$f" ) or return "$! $f" ;
   my $r = join( '', <S> ) ;
   close S ;
   return $r ;
}


sub spit($$) {
   my ( $f, $s ) = @_ ;
   open( S, ">$f" ) or die "$! $f" ;
   print S $s or die "$! $f" ;
   close S or die "$! $f" ;
}

##
## A grossly inefficient filter to test filter
## chains.  It's inefficient because we want to make sure that the
## filter chain flushing logic works.  The inefficiency is that it
## doesn't process as much input as it could each call, so lots of calls
## are required.
##
sub alt_casing_filter {
   my ( $in_ref, $out_ref ) = @_ ;
   return input_avail && do {
      $$out_ref .= lc( substr( $$in_ref, 0, 1, '' ) ) ;
      1 ;
   } && (
      ! input_avail || do {
	 $$out_ref .= uc( substr( $$in_ref, 0, 1, '' ) ) ;
         1 ;
      }
   ) ;
}


sub case_inverting_filter {
   my ( $in_ref, $out_ref ) = @_ ;
   return input_avail && do {
      $$in_ref =~ tr/a-zA-Z/A-Za-z/ ;
      $$out_ref .= $$in_ref ;
      $$in_ref = '' ;
      1 ;
   } ;
}


my $r ;


my @tests = (
##
## Internal testing
##
filter_tests(
   "alt_casing_filter",
   "Hello World",
   ["hElLo wOrLd" =~ m/(..?)/g],
   \&alt_casing_filter
),

filter_tests(
   "case_inverting_filter",
   "Hello World",
   "hELLO wORLD",
   \&case_inverting_filter
),

##
## Calling the local system's shell
##
sub { ok(   run qq{$^X -e exit}       ) },
sub { ok( $?, 0 ) },
sub { ok( ! run qq{$^X -e 'exit(42)'} ) },
sub { ok( $? ) },

##
## Simple commands, not executed via shell
##
sub { ok( run $^X, qw{-e exit}       ) },
sub { ok( $?, 0 ) },
sub { ok( ! run $^X, qw{-e exit(42)} ) },
sub { ok( $? ) },

##
## A function
##
sub { ok( run sub{} ) },
sub { ok( $?, 0 ) },
sub { ok( ! run sub{ exit(42) } ) },
sub { ok(   $? ) },

##
## A function, and an init function
##
sub {
   my $e = 0 ;
   ok(
      ! run(
         sub{ exit($e) },
         init => sub { $e = 42 }
      )
   )
},
sub { ok( $? ) },

##
## scalar ref I & O redirection using op tokens
##
sub {
   $in = $emitter_script ;
   $out = 'REPLACE ME' ;
   $err = 'REPLACE ME' ;
   $fd_map = map_fds ;
   $r = run \@perl, '<', \$in, '>', \$out, '2>', \$err ;
   ok( $r ) ;
},
sub { ok( ! $? ) },
sub { ok( map_fds, $fd_map ) },

sub { ok( $in,     $emitter_script ) },
sub { ok( $out,        $text       ) },
sub { ok( $err,    uc( $text )     ) },

##
## scalar ref I & O redirection, succinct mode.
##
sub {
   $in = $emitter_script ;
   $out = 'REPLACE ME' ;
   $err = 'REPLACE ME' ;
   $fd_map = map_fds ;
   $r = run \@perl, \$in, \$out, \$err ;
   ok( $r ) ;
},
sub { ok( ! $? ) },
sub { ok( map_fds, $fd_map ) },

sub { ok( $in,     $emitter_script ) },
sub { ok( $out,        $text       ) },
sub { ok( $err,    uc( $text )     ) },

##
## child function, scalar ref I & O redirection, succinct mode.
##
sub {
   $in = $text ;
   $out = 'REPLACE ME' ;
   $err = 'REPLACE ME' ;
   $fd_map = map_fds ;
   $r = run(
      sub { while (<>) { print ; print STDERR uc( $_ ) } },
         \$in, \$out, \$err 
      ) ;
   ok( $r ) ;
},
sub { ok( ! $? ) },
sub { ok( map_fds, $fd_map ) },

sub { ok( $in,         $text       ) },
sub { ok( $out,        $text       ) },
sub { ok( $err,    uc( $text )     ) },

##
## here document as input
##
sub {
   $out = 'REPLACE ME' ;
   $err = 'REPLACE ME' ;
   $fd_map = map_fds ;
   $r = run \@perl, \<<TOHERE, \$out, \$err ;
$emitter_script
TOHERE
   ok( $r ) ;
},
sub { ok( ! $? ) },
sub { ok( map_fds, $fd_map ) },

sub { ok( $out,    $text       ) },
sub { ok( $err,    uc( $text ) ) },

##
## undef as input
##
sub {
   $out = 'REPLACE ME' ;
   $err = 'REPLACE ME' ;
   $fd_map = map_fds ;
   $r = run \@perl, \undef, \$out, \$err ;
   ok( $r ) ;
},
sub { ok( ! $? ) },
sub { ok( map_fds, $fd_map ) },

sub { ok( $out, '' ) },
sub { ok( $err, '' ) },

##
## filehandle input redirection
##
sub {
   $out = 'REPLACE ME' ;
   $err = 'REPLACE ME' ;
   $fd_map = map_fds ;
   spit( $in_file, $emitter_script ) ;
   open( F, "<$in_file" ) or die "$! $in_file" ;
   $r = run \@perl, \*F, \$out, \$err ;
   close F ;
   unlink $in_file or warn "$! $in_file" ;
   ok( $r ) ;
},
sub { ok( ! $? ) },
sub { ok( map_fds, $fd_map ) },

sub { ok( $out,    $text       ) },
sub { ok( $err,    uc( $text ) ) },

##
## input redirection via caller writing directly to a pipe
##
sub {
   $out = 'REPLACE ME' ;
   $err = 'REPLACE ME' ;
   $fd_map = map_fds ;
   $h = start \@perl, '<pipe', \*IN, '>', \$out, '2>', \$err ;
   ## Assume this won't block...
   print IN $emitter_script ;
   close IN or warn $! ;
   $r = $h->finish ;
   ok( $r ) ;
},
sub { ok( ! $? ) },
sub { ok( map_fds, $fd_map ) },

sub { ok( $out,    $text       ) },
sub { ok( $err,    uc( $text ) ) },

##
## filehandle input redirection, passed via *F{IO}
##
sub {
   $out = 'REPLACE ME' ;
   $err = 'REPLACE ME' ;
   $fd_map = map_fds ;
   spit( $in_file, $emitter_script ) ;
   open( F, "<$in_file" ) or die "$! $in_file" ;
   $r = run \@perl, *F{IO}, \$out, \$err ;
   close F ;
   unlink $in_file or warn "$! $in_file" ;
   ok( $r ) ;
},
sub { ok( ! $? ) },
sub { ok( map_fds, $fd_map ) },

sub { ok( $out,    $text       ) },
sub { ok( $err,    uc( $text ) ) },

##
## filehandle output redirection
##
sub {
   $fd_map = map_fds ;
   open( OUT, ">$out_file" ) or die "$! $out_file" ;
   open( ERR, ">$err_file" ) or die "$! $err_file" ;
   print OUT     "out: " ;
   print ERR uc( "err: " ) ;
   $r = run \@emitter, \undef, \*OUT, \*ERR ;
   print OUT " more out data" ;
   print ERR uc( " more err data" ) ;
   close OUT ;
   close ERR ;
   $out = slurp( $out_file ) ;
   $err = slurp( $err_file ) ;
   unlink $out_file or warn "$! $out_file" ;
   unlink $err_file or warn "$! $err_file" ;
   ok( $r ) ;
},
sub { ok( ! $? ) },
sub { ok( map_fds, $fd_map ) },

sub { ok( $out,     "out: $text more out data"   ) },
sub { ok( $err, uc( "err: $text more err data" ) ) },

##
## filehandle output redirection via a pipe that is returned to the caller
##
sub {
   $fd_map = map_fds ;
   my $r = run \@emitter, \undef, '>pipe', \*OUT, '2>pipe', \*ERR ;
   $out = '' ;
   $err = '' ;
   read OUT, $out, 10000 or warn $!;
   read ERR, $err, 10000 or warn $!;
   close OUT or warn $! ;
   close ERR or warn $! ;
   ok( $r ) ;
},
sub { ok( ! $? ) },
sub { ok( map_fds, $fd_map ) },

sub { ok( $out,     $text   ) },
sub { ok( $err, uc( $text ) ) },

##
## sub I & O redirection
##
sub {
   $in = $emitter_script ;
   $out = undef ;
   $err = undef ;
   $fd_map = map_fds ;
   $r = run(
      \@perl,
      '<',  sub { my $f = $in ; $in = undef ; return $f },
      '>',  sub { $out .= shift },
      '2>', sub { $err .= shift },
   ) ;
   ok( $r ) ;
},
sub { ok( ! $? ) },
sub { ok( map_fds, $fd_map ) },

sub { ok( $out,        $text       ) },
sub { ok( $err,    uc( $text )     ) },

##
## input redirection from a file
##
sub {
   $out = undef ;
   $err = undef ;
   $fd_map = map_fds ;
   spit( $in_file, $emitter_script ) ;
   $r = run(
      \@perl,
      "<$in_file",
      '>',  sub { $out .= shift },
      '2>', sub { $err .= shift },
   ) ;
   unlink( $in_file ) or warn "$! unlinking '$in_file'" ;
   ok( $r ) ;
},
sub { ok( ! $? ) },
sub { ok( map_fds, $fd_map ) },

sub { ok( $out, $text ) },
sub { ok( $err, uc( $text ) ) },

##
## reading input from a non standard fd
##
sub {
   $out = undef ;
   $err = undef ;
   $fd_map = map_fds ;
   $r = run(
      [ @perl, '-le', 'open( STDIN, "<&3" ) or die $! ; print <STDIN>' ],
      "3<", \"Hello World",
      '>',  \$out,
      '2>', \$err,
   ) ;
   ok( $r ) ;
},
sub { ok( ! $? ) },
sub { ok( map_fds, $fd_map ) },

sub { ok( $out, $text ) },
sub { ok( $err, '' ) },

##
## duping input descriptors and an input descriptor > 0
##
sub {
   $in  = $emitter_script ;
   $out = 'REPLACE ME' ;
   $err = 'REPLACE ME' ;
   $fd_map = map_fds ;
   $r = run(
      \@perl, 
	 '>',  \$out, 
	 '2>', \$err, 
	 '3<', \$in,
	 '0<&3',
   ) ;
   ok( $r ) ;
},
sub { ok( ! $? ) },
sub { ok( map_fds, $fd_map ) },
sub { ok( $in,     $emitter_script ) },
sub { ok( $out,     $text   ) },
sub { ok( $err, uc( $text ) ) },

##
## closing input descriptors
##
sub {
   $out = 'REPLACE ME' ;
   $err = 'REPLACE ME' ;
   $fd_map = map_fds ;
   spit( $in_file, $emitter_script ) ;
   $r = run(
      [ @perl, '-e', '$l = readline *STDIN or die $! ; print $l' ], 
	 '>',  \$out, 
	 '2>', \$err, 
	 '<',  $in_file,
	 '0<&-',
   ) ;
   unlink $in_file or warn "$! unlinking '$in_file'" ;
   ok( ! $r ) ;
},
sub { ok( $? ) },
sub { ok( map_fds, $fd_map ) },
sub { ok( $out, ''  ) },
sub { ok( $err =~ /Bad file/i ) },

##
## input redirection from a non-existent file
##
sub {
   $out = 'REPLACE ME' ;
   $err = 'REPLACE ME' ;
   $fd_map = map_fds ;
   if ( -x $in_file ) {
      unlink( $in_file ) or die "$! unlinking '$in_file'" ;
   }
   eval {
      $r = run \@perl, ">$out_file", "<$in_file" ;
   } ;
   ok( $@ =~ /\Q$in_file\E/ ) ;
},
sub { ok( map_fds, $fd_map ) },

##
## output redirection to a file w/ creation or truncation
##
sub {
   $fd_map = map_fds ;
   if ( -x $out_file ) {
      unlink( $out_file ) or die "$! unlinking '$out_file'" ;
   }
   if ( -x $err_file ) {
      unlink( $err_file ) or die "$! unlinking '$err_file'" ;
   }
   $r = run(
      \@emitter,
      ">$out_file",
      "2>$err_file",
   ) ;
   $out = slurp( $out_file ) ;
   $err = slurp( $err_file ) ;
   ok( $r ) ;
},
sub { ok( ! $? ) },
sub { ok( map_fds, $fd_map ) },

sub { ok( $out, $text ) },
sub { ok( $err, uc( $text ) ) },

##
## output file redirection, w/ truncation
##
sub {
   $fd_map = map_fds ;
   spit( $out_file, 'out: ' ) ;
   spit( $err_file, 'ERR: ' ) ;
   $r = run(
      \@emitter,
      ">$out_file",
      "2>$err_file",
   ) ;
   $out = slurp( $out_file ) ;
   unlink $out_file or warn "$! unlinking '$out_file'" ;
   $err = slurp( $err_file ) ;
   unlink $err_file or warn "$! unlinking '$err_file'" ;
   ok( $r ) ;
},
sub { ok( ! $? ) },
sub { ok( map_fds, $fd_map ) },

sub { ok( $out, $text ) },
sub { ok( $err, uc( $text ) ) },

##
## output file redirection w/ append
##
sub {
   spit( $out_file, 'out: ' ) ;
   spit( $err_file, 'ERR: ' ) ;
   $fd_map = map_fds ;
   $r = run(
      \@emitter,
      ">>$out_file",
      "2>>$err_file",
   ) ;
   $out = slurp( $out_file ) ;
   unlink $out_file or warn "$! unlinking '$out_file'" ;
   $err = slurp( $err_file ) ;
   unlink $err_file or warn "$! unlinking '$err_file'" ;
   ok( $r ) ;
},
sub { ok( ! $? ) },
sub { ok( map_fds, $fd_map ) },

sub { ok( $out, "out: $text" ) },
sub { ok( $err, uc( "err: $text" ) ) },

##
## dup()ing output descriptors
##
sub {
   $out = 'REPLACE ME' ;
   $err = 'REPLACE ME' ;
   $fd_map = map_fds ;
   $r = run \@emitter, '>', \$out, '2>', \$err, '2>&1' ;
   ok( $r ) ;
},
sub { ok( ! $? ) },
sub { ok( map_fds, $fd_map ) },
sub { ok( $out =~ /(?:$text){2}/i ) },
sub { ok( $err, '' ) },

##
## stderr & stdout redirection to the same file via >&word
##
sub {
   $fd_map = map_fds ;
   if ( -x $out_file ) {
      unlink( $out_file ) or die "$! unlinking '$out_file'" ;
   }
   $r = run \@emitter, ">&$out_file" ;
   $out = slurp( $out_file ) ;
   ok( $r ) ;
},
sub { ok( ! $? ) },
sub { ok( map_fds, $fd_map ) },

sub { ok( $out =~ qr/(?:$text){2}/i ) },

##
## Non-zero exit value, command with args, no redirects.
##
sub {
   $fd_map = map_fds ;
   $r = run [ @perl, '-e', 'exit(42)' ] ;
   ok( !$r ) ;
},
sub { ok( $?, 42 << 8 ) },
sub { ok( map_fds, $fd_map ) },

##
## Zero exit value, command with args, no redirects.
##
sub {
   $fd_map = map_fds ;
   $r = run [ @perl, qw{ -e exit }] ;
   ok( $r ) ;
},
sub { ok( ! $? ) },
sub { ok( map_fds, $fd_map ) },

##
## dup()ing output descriptors that collide.
##
## This test assumes that our caller doesn't leave a lot of fds opened,
## and assumes that $out_file will be opened on fd 3, 4 or 5.
##
sub {
   $out = 'REPLACE ME' ;
   $err = 'REPLACE ME' ;
   if ( -x $out_file ) {
      unlink( $out_file ) or die "$! unlinking '$out_file'" ;
   }
   $fd_map = map_fds ;
   $r = run(
      \@emitter,
      "3>&1", "4>&1", "5>&1",
      ">$out_file",
      '2>', \$err,
   ) ;
   $out = slurp( $out_file ) ;
   unlink $out_file ;
   ok( $r ) ;
},
sub { ok( ! $? ) },
sub { ok( map_fds, $fd_map ) },
sub { ok( $out,     $text   ) },
sub { ok( $err, uc( $text ) ) },

##
## Pipelining
##
sub {
   $out = 'REPLACE ME' ;
   $err = 'REPLACE ME' ;
   $fd_map = map_fds ;
   $r = run(
      [ @perl, '-lane', 'print STDERR "1:$_" ; print uc($F[0])," ",$F[1]'],
         \"Hello World",
  '|',[ @perl, '-lane', 'print STDERR "2:$_" ; print $F[0]," ",lc($F[1])'],
	 \$out,
	 \$err,
   ) ;
   ok( $r ) ;
},
sub { ok( ! $? ) },
sub { ok( map_fds, $fd_map ) },
sub { ok( $out, "HELLO world\n" ) },
sub { ok( $err, "1:Hello World\n2:HELLO World\n" ) },

##
## Parallel (unpiplined) processes
##
sub {
   $out = 'REPLACE ME' ;
   $err = 'REPLACE ME' ;
   $fd_map = map_fds ;
   $r = run(
      [ @perl, '-lane', 'print STDERR "1:$_" ; print uc($F[0])," ",$F[1]' ],
         \"Hello World",
 '&', [ @perl, '-lane', 'print STDERR "2:$_" ; print $F[0]," ",lc( $F[1] )' ],
	 \"Hello World",
	 \$out,
	 \$err,
   ) ;
   ok( $r ) ;
},
sub { ok( ! $? ) },
sub { ok( map_fds, $fd_map ) },
sub { ok( $out =~ qr/^(?:HELLO World\n|Hello world\n){2}$/s ) },
sub { ok( $err =~ qr/^(?:[12]:Hello World.*){2}$/s ) },

##
## A few error cases...
##
sub {
   eval { $r = run \@perl, '<', {}, {} } ;
   ok( $@ =~ qr/not allowed/ ) ;
},

sub {
   eval { $r = run \@perl, '>', {}, {} } ;
   ok( $@ =~ qr/not allowed/ ) ;
},

(
   map {
      my $foo = $_ ;
      sub {
	 eval { $r = run $foo, [] } ;
	 ok( $@ =~ qr/command/ ) ;
      }
   } qw( | & < > >& 1>&2 >file <file 2<&1 <&- 3<&- )
),

sub {
   $out = 'REPLACE ME' ;
   $err = 'REPLACE ME' ;
   $fd_map = map_fds ;
   eval {
      $r = run( \@emitter, '>', \$out, '2>', \$err,
	 { _simulate_fork_failure => 1 }
      ) ;
   } ;
   ok( $@ ) ;
},
sub { ok( ! $? ) },
sub { ok( map_fds, $fd_map ) },

sub { ok( $out, '' ) },
sub { ok( $err, '' ) },

sub {
   $fd_map = map_fds ;
   eval {
      $r = run \@perl, '<file', {_simulate_open_failure => 1} ;
   } ;
   ok( $@ ) ;
},
sub { ok( ! $? ) },
sub { ok( map_fds, $fd_map ) },

sub {
   $fd_map = map_fds ;
   eval {
      $r = run \@perl, '>file', {_simulate_open_failure => 1} ;
   } ;
   ok( $@ ) ;
},
sub { ok( ! $? ) },
sub { ok( map_fds, $fd_map ) },

##
## harness, pump, run
##
sub {
   $in  = 'SHOULD BE UNCHANGED' ;
   $out = 'REPLACE ME' ;
   $err = 'REPLACE ME' ;
   $? = 99 ;
   $fd_map = map_fds ;
   $h = start(
      [ @perl, '-pe', 'BEGIN { $| = 1 } print STDERR uc($_)' ],
      \$in, \$out, \$err,
   ) ;
   ok( isa( $h, 'IPC::Run' ) ) ;
},
sub { ok( $?, 99 ) },

sub { ok( $in,  'SHOULD BE UNCHANGED' ) },
sub { ok( $out, '' ) },
sub { ok( $err, '' ) },
sub { ok( $h->pumpable ) },

sub {
   $in  = '' ;
   $? = 0 ;
   pump_nb $h for ( 1..100 ) ;
   ok( 1 ) ;
},
sub { ok( $in, '' ) },
sub { ok( $out, '' ) },
sub { ok( $err, '' ) },
sub { ok( $h->pumpable ) },

sub {
   $in  = $text ;
   $? = 0 ;
   pump $h until $out =~ /Hello World/ ;
   ok( 1 ) ;
},
sub { ok( ! $? ) },
sub { ok( $in, '' ) },
sub { ok( $out, $text ) },
sub { ok( $h->pumpable ) },

sub { ok( $h->finish ) },
sub { ok( ! $? ) },
sub { ok( map_fds, $fd_map ) },
sub { ok( $out,     $text   ) },
sub { ok( $err, uc( $text ) ) },
sub { ok( ! $h->pumpable ) },

##
## start, run, run, run.  See Tom run.  A do-run-run, a-do-run-run.
##
sub {
   $in  = 'SHOULD BE UNCHANGED' ;
   $out = 'REPLACE ME' ;
   $err = 'REPLACE ME' ;
   $fd_map = map_fds ;
   $h = start(
      [ @perl, '-pe', 'BEGIN { $| = 1 } print STDERR uc($_)' ],
	 \$in, \$out, \$err,
   ) ;
   ok( isa( $h, 'IPC::Run' ) ) ;
},

sub { ok( $in, 'SHOULD BE UNCHANGED' ) },
sub { ok( $out, '' ) },
sub { ok( $err, '' ) },
sub { ok( $h->pumpable ) },

sub { 
   $in  = $text ;
   ok( $h->finish )
},
sub { ok( ! $? ) },
sub { ok( map_fds, $fd_map ) },
sub { ok( $in,      ''      ) },
sub { ok( $out,     $text   ) },
sub { ok( $err, uc( $text ) ) },
sub { ok( ! $h->pumpable ) },

sub { 
   $in  = $text ;
   $out = 'REPLACE ME' ;
   $err = 'REPLACE ME' ;
   ok( $h->run )
},
sub { ok( ! $? ) },
sub { ok( map_fds, $fd_map ) },
sub { ok( $in,      $text   ) },
sub { ok( $out,     $text   ) },
sub { ok( $err, uc( $text ) ) },
sub { ok( ! $h->pumpable ) },

sub { 
   $in  = $text ;
   $out = 'REPLACE ME' ;
   $err = 'REPLACE ME' ;
   ok( $h->run )
},
sub { ok( ! $? ) },
sub { ok( map_fds, $fd_map ) },
sub { ok( $in,      $text   ) },
sub { ok( $out,     $text   ) },
sub { ok( $err, uc( $text ) ) },
sub { ok( ! $h->pumpable ) },

##
## Output filters
##
sub {
   $out = 'REPLACE ME' ;
   $err = 'REPLACE ME' ;
   $fd_map = map_fds ;
   $r = run(
      \@emitter,
      '>',
         \&alt_casing_filter,
	 \&case_inverting_filter,
	 \$out,
      '2>', \$err,
   ) ;
   ok( $r ) ;
},
sub { ok( ! $? ) },
sub { ok( map_fds, $fd_map ) },

sub { ok( $out, "HeLlO WoRlD\n" ) },
sub { ok( $err, uc( $text ) ) },

##
## Input filters
##
sub {
   $out = 'REPLACE ME' ;
   $err = 'REPLACE ME' ;
   $fd_map = map_fds ;
   $in = $text ;
   $r = run(
      [ @perl, '-pe', 'print STDERR uc $_' ],
      '0<',
         \&case_inverting_filter,
	 \&alt_casing_filter,
	 \$in,
      '1>', \$out,
      '2>', \$err,
   ) ;
   ok( $r ) ;
},
sub { ok( ! $? ) },
sub { ok( map_fds, $fd_map ) },

sub { ok( $in,      $text   ) },
sub { ok( $out,    "HeLlO WoRlD\n" ) },
sub { ok( $err,    uc( $text ) ) },
) ;

plan tests => scalar @tests ;

$_->() for ( @tests ) ;
