package IPC::Run ;

#
# Copyright (c) 1999 by Barrie Slaymaker, barries@slaysys.com
#
# You may distribute under the terms of either the GNU General Public
# License or the Artistic License, as specified in the README file.
#

=head1 NAME

IPC::Run - run processes with piping and redirection

=head1 SYNOPSIS

   use IPC::Run qw( run   harness start pump finish ) ;

   my @cat = qw( cat in.txt - ) ;

   # Batch process fed from / outputting to scalars
      $in = "input" ;
      run \@cat, \$in, \$out, \$err or die "cat returned $?";

   # Incrementally read from / write to scalars.  Note that $in_q
   # is a queue that is drained as it is used, which is not the
   # way run() treats inputs.  $h is for "harness".
      my $h = start(
         \@cat, \$in_q, \$out_q, \$err_q,
	 { timeout => 10 }
      ) ;

      $in_q .= "some input\n" ;
      pump $h until $out_q =~ /input/ ;
      warn $err_q ; 
      $err_q = '' ;
      $out_q = '' ;

      $in_q .= "some more input\n" ;
      pump $h until $out_q =~ /input/ ;
      warn $err_q ; 
      $err_q = '' ;

      $in_q .= "some final input\n" ;
      $h->timeout( "1:00" ) ;
      finish $h or die "cat returned $?" ;
      warn $err_q ; 
      print $out_q ;

   # Batch process with stdin closed in child
      run \@cat, \undef, \$out, \$err ;

   # Batch process reading from a here document
      run \@cat, \<<IN ;
      blah
      IN

   # Batch process fed from / outputing to subroutines
      run \@cat, \&get_some_in, \&catch_some_out, \&catch_some_err ;

   # ... from / to named files (names are not munged in any way)
      run \@cat, '<', $in_name, '>', $out_name, '2>', $err_name ;

   # Piping between children
      run \@cat, '|', [qw(gzip)] ;

   # Multiple children simultaneously (run() blocks until all
   # children exit, use start() for background execution):
      run [qw(foo1)], '&', [qw(foo2)] ;

   # Spawning a subroutine (only on systems with true fork()/exec())
      run \&child_sub, \$in, \$out ;

   # Calling \&set_up_child in the child before it executes the
   # command (only works on systems with true fork() & exec())
      run \@cat, \$in, \$out,
         init => \&set_up_child ;

   # Append output to named files
      run( \@cat, '>>', $out_f_name, '2>>', $err_f_name ) ;

   # Read from / write to file handles you open and close
      open IN,  '<in.txt'  or die $! ;
      open OUT, '>out.txt' or die $! ;
      print OUT "preamble\n" ;
      run \@cat, \*IN, \*OUT or die "cat returned $?" ;
      print OUT "postamble\n" ;
      close IN ;
      close OUT ;

   # Create pipes for you to read / write
      $h = start \@cat, \*IN, \*OUT or die "cat returned $?" ;
      print IN "some input\n" ;
      close IN ;
      print <OUT> ;
      finish $h ;

   # Mixing input and output modes
      run \@cat, 'in.txt', \&catch_some_out, \*ERR_LOG ) ;

   # Other redirection constructs
      run \@cat, '>&', \$out_and_err ;
      run \@cat, '2>&1' ;
      run \@cat, '0<&3' ;
      run \@cat, '<&-' ;
      run \@cat, '3<', \$in3 ;
      run \@cat, '4>', \$out4 ;
      # etc.

   # Passing options:
      run \@cat, 'in.txt', { debug => 1 } ;

   # Call this system's shell, returns TRUE on 0 exit code
   # THIS IS THE OPPOSITE SENSE OF system()'s RETURN VALUE
      run "cat a b c" or die "cat returned $?" ;

   # Launch a sub process directly, no shell.  Can't do redirection
   # with this form, it's here to behave like system() with an
   # inverted result.
      $r = run "cat a b c" ;

=head1 DESCRIPTION

IPC::Run allows you run one or more child processes while optionally
redirecting input and output file descriptors to variables, subroutines,
pipes, or files.
Various redirection operators reminiscent of those
seen on common Unix and MSDOS command lines are provided.

=head2 run() vs. start()/pump()/finish()

There are two modes you can run child processes in: batch and 
incremental.

In batch mode, all input is set up for the child process
in advance, the child is run and delivers all output, and the parent
waits for the child to complete.  Batch operation is provided by run():

   run \@cmd, \<<IN, \$out ;
   blah
   IN

Incremental mode is provided by start(), pump(), and finish(): the child
process is started, and input and
output may be pump()ed in and out of the child process as needed:

   my $h = start \@cat, \$in_q, \$out_q, \$err_q ;
   $in_q = "first input\n" ;

   pump $h while length $in_q ;
   warn $err_q if length $err_q ; 
   $err_q = '' ;

   $in_q = "second input\n" ;
   pump $h until $out_q =~ /second input/ ;

   $h->finish ;

You can compile the harness with harness() prior to start()ing, and you
may omit start() between harness() and pump().  You might want
to do these things if you compile your harnesses ahead of time.

You may call timeout() at any time in this process.  If it is called
before start() (or the first pump(), if start() is omitted), then the
timer is started just after all the child processes have been fork()ed
or spawn()ed.

If you call it once the children have been start()ed, the timeout timer
is restarted.  So this sequence:

   my $h = harness \@cat, \$in_q, \$out_q, \$err_q ;
   $h->timeout( 10 ) ;
   sleep 11 ;  # No effect: timer not running yet

   start $h ;
   $in_q = "foo\n" ;
   pump $h until ! length $in_q ;
   ## 10 (or little more) may pass til this line.

   $in_q = "bar\n" ;
   $h->timeout( 1 ) ;
   ## Timer is now running, but will elapse after 1 second
   pump $h until ! length $in_q ;
   ## 1 seconds (or a little more) may pass til this line.

   $h->timeout( 30 ) ;
   $h->finish ;

The timeout does not include the time it takes to wait for the children
to exit, either.

=head2 Syntax

run(), start(), and harness() can all take a list of harness specifications
as input.  A harness specification is either a single string to be passed
to the systems' shell:

   run "echo 'hi there'" ;

or a list of commands to execute
separated by process control operators '|', and '&'.  Commands are
passed in as array references or (on systems supporting fork) as 
Perl subs:

   run \@cmd ;
   run \@cmd1, '|', \@cmd2 ;
   run \@cmd1, '&', \@cmd2 ;
   run \&sub1 ;
   run \&sub1, '|', \&sub2 ;
   run \&sub1, '&', \&sub2 ;

'|' pipes the stdout of \@cmd1 the stdin of \@cmd2, just like a
shell pipe.  '&' does not.  Child processes to the right of each '&'
will have their stdin closed unless it's redirected-to.

Each command can have redirection specifiers after it and before any
'|' or '&'.  These may be succinct:

   run \@cmd, \undef, \$out ;
   run \@cmd, \$in, \$out ;
   run \@cmd1, \$in, '|', \@cmd2, \$out ;

or verbose, using operators found on many shell command lines:

   run \@cmd, '<', \undef, '>', \$out ;
   run \@cmd, '<', \undef, '>&', \$out ;
   run(
      \@cmd1,
         '<', \$in,
      '|', \@cmd2,
         \$out
   ) ;

After each \@cmd (or \&foo), parsing begins in succinct mode and toggles to
verbose mode when an operator (ie plain scalar) is seen.  Once in
verbose mode, parseing only reverts to succinct mode when a '|' or
'&' is seen.

In succinct mode, each parameter after the \@cmd specifies what to
do with the next highest file descriptor. These File descriptor start
with 0 (stdin) unless stdin is being piped to (C<'|', \@cmd>), in which
case they start with 1 (stdout).  Currently, being on the left of
a pipe (C<\@cmd, \$out, \$err, '|'>) does I<not> cause stdout to be
skipped, though this may change since it's not as DWIMerly as it
could be.  Only stdin is assumed to be an
input in succinct mode, all others are assumed to be outputs.

If no piping or redirection is specified for a child, it will
inherit the parent's open file descriptors as dictated by your
system's close-on-exec behavior and the $^F flag, except that
processes after a '&' will not inherit the parent's stdin.  Such
processes will have their stdin closed unless it has been redirected-to.

If you want to close a child processes stdin, you may do any of:

   run \@cmd, \undef ;
   run \@cmd, \"" ;
   run \@cmd, '<&-' ;
   run \@cmd, '0<&-' ;

Redirection is done by placing redirection specifications immediately 
after a command or child subroutine:

   run \@cmd1,      \$in, '|', \@cmd2,      \$out ;
   run \@cmd1, '<', \$in, '|', \@cmd2, '>', \$out ;

If you omit the redirection operators, descriptors are counted
starting at 0.  Descriptor 0 is assumed to be input, all others
are outputs.  A leading '|' consumes descriptor 0, so this
works as expected.

   run \@cmd1, \$in, '|', \@cmd2, \$out ;
   
The parameter following a redirection token can be a scalar ref,
a subroutine ref, a file name, an open filehandle, or a closed
filehandle.

If it's a scalar ref, the child reads input from or sends output to
that variable:

   $in = "Hello World.\n" ;
   run \@cat, \$in, \$out ;
   print $out ;

Scalars used in incremental (start()/pump()/finish()) applications are treated
as queues: input is removed from input scalers, resulting in them dwindling
to '', and output is appended to output scalars.  This is not true of 
harnesses run() in batch mode.

It's usually wise to append new input to be sent to the child to the input
queue, and you'll often want to zap output queues to '' before pumping.

   $h = start \@cat, \$in_q ;
   $in_q = "line 1\n" ;
   pump $h ;
   $in_q .= "line 2\n" ;
   pump $h ;
   $in_q .= "line 3\n" ;
   finish $h ;

The final call to finish() must be there: it allows the child process(es)
to run to completion and waits for their exit values.

=head2 Redirection Operators

=over

=item Redirecting input: [n]<

You can input the child reads on file descriptor number n to come from a
scalar variable, subroutine, file handle, or a named file.  If stdin
is not redirected, the parent's stdin is inherited.

   run \@cat, \undef          ## Closes child's stdin immediately
      or die "cat returned $?" ; 

   run \@cat, \$in ;

   run \@cat, \<<TOHERE ;
   blah
   TOHERE

   run \@cat, \&input ;       ## Calls &input, feeding data returned
                              ## to child's.  Closes child's stdin
			      ## when undef is returned.

Redirecting from named files requires you to use the input
redirection operator:

   run \@cat, '<.profile' ;
   run \@cat, '<', '.profile' ;

   open IN, "<foo" ;
   run \@cat, \*IN ;
   run \@cat, *IN{IO} ;

The form used second example here is the safest,
since filenames like "0" and "&more\n" won't confuse &run:

You can't do either of

   run \@a, *IN ;      ## INVALID
   run \@a, '<', *IN ; ## BUGGY: Reads file named like "*main::A"
   
because perl passes a scalar containing a string that
looks like "*main::A" to &run, and &run can't tell the difference
between that and a redirection operator or a file name.  &run guarantees
that any scalar you pass after a redirection operator is a file name.

If your child process will take input from file descriptors other
than 0 (stdin), you can use a redirection operator with any of the
valid input forms (scalar ref, sub ref, etc.):

   run \@cat, '3<', \$in3_q ;

When redirecting input from a scalar ref, the scalar ref is
used as a queue.  This allows you to use &harness and pump() to
feed incremental bits of input to a coprocess.  See L</Coprocesses>
below for more information.

=item Redirecting output: [n]>, [n]>>, [n]>&[m]

You can redirect any output the child emits
to a scalar variable, subroutine, file handle, or file name.  You
can have &run truncate or append to named files or scalars.  If
you are redirecting stdin as well, or if the command is on the
receiving end of a pipeline ('|'), you can omit the redirection
operator:

   @ls = ( 'ls' ) ;
   run \@ls, \undef, \$out
      or die "ls returned $?" ; 

   run \@ls, \undef, \&out ;  ## Calls &out each time some output
                              ## is received from the child's 
			      ## when undef is returned.

   run \@ls, \undef, '2>ls.err' ;
   run \@ls, '2>', 'ls.err' ;

The two parameter form guarantees that the filename
will not be interpreted as a redirection operator:

   run \@ls, '>', "&more" ;
   run \@ls, '2>', ">foo\n" ;

You can pass file handles you've opened for writing:

   open( *OUT, ">out.txt" ) ;
   open( *ERR, ">err.txt" ) ;
   run \@cat, \*OUT, \*ERR ;

Passing a scalar reference and a code reference requires a little
more work, but allows you to capture all of the output in a scalar
or each piece of output by a callback:

These two do the same things:

   run( [ 'ls' ], '2>', sub { $err_out .= $_[0] } ) ;

does the same basic thing as:

   run( [ 'ls' ], '2>', \$err_out ) ;

The subroutine will be called each time some data is read from the child.

=item Duplicating output descriptors: >&m, n>&m

This duplicates output descriptor number n (default is 1 if n is ommitted)
from descriptor number m.

=item Duplicating input descriptors: <&m, n<&m

This duplicates input descriptor number n (default is 0 if n is ommitted)
from descriptor number m

=item Closing descriptors: <&-, 3<&-

This closes descriptor number n (default is 0 if n is ommitted).  The
following commands are equivalent:

   run \@cmd, \undef ;
   run \@cmd, '<&-' ;
   run \@cmd, '<in.txt', '<&-' ;

Doing

   run \@cmd, \$in, '<&-' ;    ## SIGPIPE recipe.

is dangerous: the parent will get a SIGPIPE if $in is not empty.

=item Redirecting both stdout and stderr

The following pairs of commands are equivalent:

   run \@cmd, '>&', \$out ;       run \@cmd, '>', \$out,     '2>&1' ;
   run \@cmd, '>&', 'out.txt' ;   run \@cmd, '>', 'out.txt', '2>&1' ;

etc.

=item Redirection Filters

Both input redirections and output redirections that use scalars or
subs as endpoints may have an arbitrary number of filter subs placed
between them and the child process.  This is useful if you want to
receive output in chunks, or if you want to massage each chunk of
data sent to the child.  To use this feature, you must use verbose
syntax:

   run(
      \@cmd
         '<', \&in_filter_2, \&in_filter_1, $in,
         '>', \&out_filter_1, \&in_filter_2, $out,
   ) ;

This capability is not provided for IO handles or named files.

Two filters are provided by IPC::Run: appender and chunker.  Because
these may take an argument, you need to use the constructor fuinctions
new_appender() and new_chunker() rather than using \& syntax:

   run(
      \@cmd
         '<', new_appender( "\n" ), $in,
         '>', new_chunker, $out,
   ) ;

=back

=head2 Options

Options are passed in a hash as the final parameter to run():

   run \@cat, \$in, { debug => 1 } ;

=over

=item debug

Enables debugging output in parent and child.  Children emit debugging
info over a separate, dedicated pipe to the parent, so you must pump(),
pump_nb(), or finish() if you use start().  This pipe is not created
unless debugging is enabled.

=item timeout

Same as calling timeout() immediately before the run(), harness(), or 
start() calls it is passed to.

=back

=head1 RETURN VALUES

harness() and start() return a reference to an IPC::Run harness.  This is
blessed in to the IPC::Run package, so you may make later calls to
functions as members if you like:

   $h = harness( ... ) ;
   start $h ;    # same as $h->start;
   finish $h ;   # same as $h->finish ;

Of course, using method call syntax lets you deal with any IPC::Run
subclasses that might crop up, but don't hold your breath waiting for
any.

run() and finish() return TRUE when all subcommands exit with a 0 result
code.  B<This is the opposite of perl's system() command>.

All routines raise exceptions (via die()) when error conditions are
recognized.  A non-zero command result is not treated as an error
condition, since some commands are tests whose results are reported 
in their exit codes.

=head1 ROUTINES

=over

=cut

use strict ;
use vars qw( $VERSION @ISA @EXPORT_OK %EXPORT_TAGS $debug ) ;

use Exporter ;
use Fcntl ;
use POSIX () ;
use Symbol ;

$VERSION = '0.1' ;

@ISA = qw( Exporter ) ;

## We use @EXPORT for the end user's convenience: there's only one function
## exported, it's homonymous with the module, it's an unusual name, and
## it can be suppressed by "use IPC::Run () ;".

my @FILTER_IMP = qw( input_avail get_more_input ) ;
my @FILTERS    = qw(
   new_appender
   new_chunker
   new_string_source
   new_string_sink
) ;
my @API        = qw( run   harness start pump finish ) ;

@EXPORT_OK = ( @API, @FILTER_IMP, @FILTERS, qw( filter_tests ) ) ;
%EXPORT_TAGS = (
   'filter_imp' => \@FILTER_IMP,
   'all'        => \@EXPORT_OK,
   'filters'    => \@FILTERS,
   'api'        => \@API,
) ;

use Carp ;
use Errno qw( EAGAIN ) ;
use File::Spec ;
use FileHandle ;
use UNIVERSAL qw( isa ) ;

$debug = 0 ;

sub input_avail() ;
sub get_more_input() ;

###############################################################################

##
## State machine states
##
## These must be in ascending order numerically
sub _zippo()    {0}
sub _harnessed(){1}
sub _started()  {2}

##
## Debugging routines
##
sub _map_fds {
   my $map = '' ;
   my $digit = 0 ;
   my $in_use ;
   for my $fd (0..63) {
      ## I'd like a quicker way to detect open file descriptors
      my $test_fd = POSIX::dup( $fd ) ;
      $in_use = defined $test_fd ;
      POSIX::close $test_fd if $in_use ;
      $map .= $in_use ? $digit : '-';
      $digit = 0 if ++$digit > 9 ;
   }
   warn "No fds open???" unless $map =~ /1/ ;
   $map =~ s/(.{1,12})-*$/$1/ ;
   return $map ;
}


my $parent_pid = $$ ;

## We emit our debugging msgs on this fd.  It's changed in the kids so
## that the parent can catch them and emit them (in case kid's STDERR is
## redirected).
my $debug_fd = fileno STDERR ;

## Kids send to our select loop on this fd.
my $recv_debug_fd ;

sub debug {
   my $self = shift ;
   return unless $self->{debug} ;
   my $s ;
   my $prefix = join(
      '',
      'harness',
      ( $$ eq $parent_pid ? () : ( '[', $$, ']' ) ),
      ': ',
      ( $self->{debug} > 1 ? ( _map_fds, ' ' ) : () ),
   ) ;

   my $msg = join( '', map { defined $_ ? $_ : "<undef>" } @_ ) ;
   chomp $msg ;
   $msg =~ s{^}{$prefix}gm ;
   $msg .= "\n" ;
   POSIX::write( $debug_fd, $msg, length $msg ) ;
}


my @fd_descs = ( 'stdin', 'stdout', 'stderr' ) ;

sub debug_fd {
   my $self = shift ;
   return unless $self->{debug} ;

   my $text = shift ;
   my $op = pop ;
   my $kid = $_[0] ;


   $self->debug(
      $text,
      ' ',
      ( defined $op->{FD}
         ? $op->{FD} < 3
	    ? ( $fd_descs[$op->{FD}] )
	    : $op->{FD} eq $recv_debug_fd
	       ? ( 'debug (', $op->{FD}, ')' )
	       : ( 'fd ', $op->{FD} )
	 : $op->{FD}
      ),
      ' (kid',
      ( defined $kid
	 ? ( ' ', $kid->{NUM}, )
	 : ()
      ),
      "'s ",
      ( defined $op->{KFD}
	 ? $op->{KFD} < 3
	    ? $fd_descs[$op->{KFD}]
	    : $op->{KFD} == $recv_debug_fd
	       ? ( 'debug (', $op->{KFD}, ')' )
	       : ( 'fd ', $op->{KFD} )
	 : $op->{KFD}
      ),
      ')',
   ) ;
}

##
## Support routines
##

my %cmd_cache ;

sub _search_path {
   my $self = shift ;
   my ( $cmd_name ) = @_ ;
   if ( File::Spec->file_name_is_absolute( $cmd_name ) ) {
      $self->debug( "'", $cmd_name, "' is absolute" ) ;
      return $cmd_name ;
   }
   if ( exists $cmd_cache{$cmd_name} ) {
      $self->debug( "'", $cmd_name, "' found in cache: '", $cmd_cache{$cmd_name}, "'");
      return $cmd_cache{$cmd_name} if -x $cmd_cache{$cmd_name} ;
      $self->debug( "'", $cmd_cache{$cmd_name},"' no longer executable, searching...");
   }

   my @searched_in ;

   unless ( exists $cmd_cache{$cmd_name} ) {
      ## This next bit is Unix specific, unfortunately.
      ## There's been some conversation about extending File::Spec to provide
      ## a universal interface to PATH, but I haven't seen it yet.
      my $re ;
      for ( $^O ) {
         if ( /Win32/ )         { $re = qr/;/ }
         else                   { $re = qr/:/ }
      }
   LOOP:
      for ( split( $re, $ENV{PATH}, -1 ) ) {
	 $_ = "." unless length $_ ;
	 push @searched_in, $_ ;

	 my $prospect = File::Spec->catfile( $_, $cmd_name ) ;
         my @prospects ;
         ## TODO: Use a better algorithm for finding executables on
         ## non-Unix.  Maybe defer to system().
         for ( $^O ) {
            if ( /Win32/ ) {
               @prospects = -f $prospect
                  ? ( $prospect )
                  : ( $prospect, glob( "$prospect.*" ) )  ;
            }
            else {
               @prospects = ( $prospect ) ;
            }
         }
         for my $found ( @prospects ) {
            if ( -f $found && -x _ ) {
	       $self->debug( 'found ', $found ) ;
	       $cmd_cache{$cmd_name} = $found ;
	       last LOOP ;
	    }
         }
      }
   }
   if ( exists $cmd_cache{$cmd_name} ) {
      $self->debug( "'", $cmd_name, "' added to cache: '", $cmd_cache{$cmd_name}, "'");
      return $cmd_cache{$cmd_name} ;
   }

   croak "Command '$cmd_name' not found in " . join( ", ", @searched_in ) ;
}


sub _empty($) { ! ( defined $_[0] && length $_[0] ) }


##
## 'safe' versions of otherwise fun things to do
##
sub _close {
   my $self = shift ;
   confess 'undef' unless defined $_[0] ;
   no strict 'refs' ;
   my $fn = $_[0] =~ /^\d+$/ ? $_[0] : fileno $_[0] ;
   my $r = POSIX::close $_[0] ;
   $r = $r ? '' : " ERROR $!" ;
   $self->debug( "close( $fn )$r" ) if $self->{debug} > 1 ;
}

sub _dup {
   my $self = shift ;
   confess 'undef' unless defined $_[0] ;
   my $r = POSIX::dup( $_[0] ) ;
   croak "$!: dup( $_[0] )" unless defined $r ;
   $r = 0 if $r eq '0 but true' ;
   $self->debug( "dup( $_[0] ) = $r" ) if $self->{debug} > 1 ;
   return $r ;
}


sub _dup2 {
   my $self = shift ;
   confess 'undef' unless defined $_[0] && defined $_[1] ;
   my $r = POSIX::dup2( $_[0], $_[1] ) ;
   croak "$!: dup2( $_[0], $_[1] )" unless defined $r ;
   $r = 0 if $r eq '0 but true' ;
   $self->debug( "dup2( $_[0], $_[1] ) = $r" ) if $self->{debug} > 1 ;
   return $r ;
}

sub _exec {
   my $self = shift ;
   confess 'undef' unless defined $_[0] ;
   exec @_ or croak "$!: exec( " . join( ', ', @_ ) . " )" ;
}

sub _sysopen {
   my $self = shift ;
   confess 'undef' unless defined $_[0] && defined $_[1] ;
   my $r = POSIX::open( $_[0], $_[1], 0644 ) ;
   croak "$!: open( $_[0], $_[1] )" unless defined $r ;
   $self->debug( "open( $_[0], $_[1] ) = $r" ) if $self->{debug} > 1 ;
   return $r ;
}

sub _pipe {
   my $self = shift ;
   my ( $r, $w ) = POSIX::pipe ;
   croak "$!: pipe()" unless $r ;
   $self->debug( "pipe() = ( $r, $w ) " ) if $self->{debug} > 1 ;
   return ( $r, $w ) ;
}


sub _read {
   my $self = shift ;
   confess 'undef' unless defined $_[0] ;
   my $s  = '' ;
   my $r = POSIX::read( $_[0], $s, 100000 ) ;
   croak "$!: read( $_[0] )" unless $r ;
   $self->debug( "read( $_[0] ) = '$s'" ) if $self->{debug} > 1 ;
   return $s ;
}


sub _write {
   my $self = shift ;
   confess 'undef' unless defined $_[0] && defined $_[1] ;
   my $r = POSIX::write( $_[0], $_[1], length $_[1] ) ;
   croak "$!: write( $_[0], '$_[1]' )" unless $r ;
   $self->debug( "write( $_[0], '$_[1]' ) = $r" ) if $self->{debug} > 1 ;
   return $r ;
}


=item run

Run takes a harness or harness specification and runs it, pumping
all input to the child(ren), closing the input pipes when no more
input is available, collecting all output that arrives, until the
pipes delivering output are closed, then waiting for the children to
exit and reaping their result codes.

You may think of C<run( ... )> as being like 

   start( ... )->finish() ;

if you like, though there is one subtle difference: run() does not
empty input values.

=cut

sub run {
   my $self = start( @_ ) ;
   $self->{clear_ins} = 0 ;
   $self->finish ;
}


=item harness

Takes a harness specification and returns a harness.  This harness is
blessed in to IPC::Run, allowing you to use method call syntax for
run(), start(), et al if you like.

harness() is provided so that you can pre-build harnesses if you
would like.  It's not necessary otherwise, since start() calls
harness() if it needs to.

You may call run(), start() or pump() after calling harness()
(or after calling run() or finish()).

=cut

##
## Notes: I've avoided handling a scalar that doesn't look like an
## opcode as a here document or as a filename, though I could DWIM
## those.  I'm not sure that the advantages outweight the danger when
## the DWIMer guess wrong.
##
sub harness {
   my $options = ref $_[-1] eq 'HASH' ? pop : {} ;
   $options->{debug} = $debug || 0 unless defined $options->{debug} ;

   my @args ;

   if ( @_ == 1 && ! ref $_[0] ) {
      my @shell ;
      for ( $^O ) {
         if ( /Win32/ )     { @shell = qw( command /c ) }
         else               { @shell = qw( sh -c      ) }
      }
      @args = ( [ @shell, @_ ] ) ;
   }
   elsif ( @_ > 1 && ! grep { ref $_ } @_ ) {
      @args = ( [ @_ ] ) ;
   }
   else {
      @args = @_ ;
   }

   my @errs ;
   my @kids ;

   my $ops_required ;
   my $assumed_fd = 0 ;
   my $num = 0 ;
   my $cur_kid ;

   my $self = bless {
         %$options,
	 KIDS       => \@kids,
	 PIPES      => [],
	 STATE      => _zippo,
      },
      __PACKAGE__ ;

   $self->debug( "** harnessing" ) ;

   my $first_parse ;
   local $_ ;
   while ( @args ) { for ( shift @args ) {
      eval {
         $first_parse = 1 ;

         $self-> debug(
	    "parsing ",
	    defined $_
	       ? ref $_ eq 'ARRAY'
	          ? ( '[ ', join( ', ', map{ "'$_'" } @$_ ), ' ]' )
		  : ( ref $_
		     || ( length $_ < 10
			   ? "'$_'"
			   : join( '', "'", substr( $_, 0, 10 ), "...'" )
			)
		  )
	       : '<undef>'
	 ) if $options->{debug} > 1 ;

      REPARSE:
	 if ( ref eq 'ARRAY' || ( ! $cur_kid && ref eq 'CODE' ) ) {
	    croak "Process control symbol ('|', '&') missing" if $cur_kid ;
	    $cur_kid = {
	       TYPE => 'cmd',
	       VAL  => $_,
	       NUM  => ++$num,
	       OPS  => [],
	       PID  => '',
	    } ;
	    push @kids, $cur_kid ;
	    $ops_required = 0 ;
	 }
	 elsif ( /^(\d*)>&(\d+)$/ ) {
	    croak "No command before '$_'\n" unless $cur_kid ;
	    push @{$cur_kid->{OPS}}, {
	       TYPE => 'dup',
	       KFD1 => $2,
	       KFD2 => length $1 ? $1 : 1,
	    } ;
	    $self->debug( "redirect tokens now required" ) ;
	    $ops_required = $first_parse ;
	 }
	 elsif ( /^(\d*)<&(\d+)$/ ) {
	    croak "No command before '$_'\n" unless $cur_kid ;
	    push @{$cur_kid->{OPS}}, {
	       TYPE => 'dup',
	       KFD1 => $2,
	       KFD2 => length $1 ? $1 : 0,
	    } ;
	    $ops_required = $first_parse ;
	 }
	 elsif ( /^(\d*)<&-$/ ) {
	    croak "No command before '$_'\n" unless $cur_kid ;
	    push @{$cur_kid->{OPS}}, {
	       TYPE => 'close',
	       KFD  => length $1 ? $1 : 0,
	    } ;
	    $ops_required = $first_parse ;
	 }
	 elsif ( /^(\d*)<(.*)$/ ) {
	    croak "No command before '$_'\n" unless $cur_kid ;

	    $ops_required = $first_parse ;

	    my $pipe = {
	       TYPE    => '<',
	       KFD     => length $1 ? $1 : 0,
	    } ;

	    push @{$cur_kid->{OPS}}, $pipe ;

	    if ( length $2 ) {
	       $pipe->{SOURCE} = $2 ;
	    }
	    else {
	       my @filters ;
	       if ( $ops_required ) {
		  push @filters, shift @args
		     while @args > 1 && ref $args[1] ;
	       }
	       $pipe->{SOURCE} = shift @args ;
	       croak "'$_' missing a source\n" if _empty $pipe->{SOURCE} ;

	       $self->debug(
		  'Kid ',
		  $cur_kid->{NUM},
		  "'s input fd ",
		  $pipe->{KFD},
		  ' has ',
		  scalar( @filters ),
		  ' user filters.'
	       ) if @filters ;

	       if ( ref $pipe->{SOURCE} eq 'GLOB' 
		 || isa( $pipe->{SOURCE}, 'IO::Handle' ) 
	       ) {
	          if ( ! defined fileno $pipe->{SOURCE} ) {
		     my ( $r, $w ) = $self->_pipe() ;
		     open( $pipe->{SOURCE}, ">&=$w" )
		        or croak "$! on write end of pipe" ;

		     $pipe->{SOURCE} = $r ;
		     $pipe->{EXT_PIPE} = 1 ;
		  }
		  else {
		     $pipe->{DONT_CLOSE} = 1 ; ## this FD is not closed by us.
		  }
	       }
	       elsif ( isa( $pipe->{SOURCE}, 'CODE' ) ) {
		  push(
		     @filters,
		     sub {
			my ( $in_ref, $out_ref ) = @_ ;

			confess '$out_ref not a SCALAR ref'
			   unless ref $out_ref eq 'SCALAR' ;

			return 0 if length $$out_ref ;

			return undef
			   if $pipe->{NO_MORE_SOURCE} ;

			my $in = $pipe->{SOURCE}->() ;
			unless ( defined $in ) {
			   $pipe->{NO_MORE_SOURCE} = 1 ;
			   return undef 
			}
			return 0 unless length $in ;
			$$out_ref = $in ;

			return 1 ;
		     }
		  ) ;
	       }
	       elsif ( isa( $pipe->{SOURCE}, 'SCALAR' ) ) {
		  push(
		     @filters,
		     sub {
			my ( $in_ref, $out_ref ) = @_ ;

			return 0 if length $$out_ref ;

			## This next bit should only happen if the user
			## passes in \undef.
			return undef if ! defined $$in_ref ;

			## no_close_ins is set/cleared by run() and pump()
			return $self->{no_close_ins} ? 0 : undef
			   if ! length ${$pipe->{SOURCE}}
			      || $pipe->{NO_MORE_SOURCE} ;

			$$out_ref = ${$pipe->{SOURCE}} ;
			eval { ${$pipe->{SOURCE}} = '' }
			   if $self->{clear_ins} ;
			$pipe->{NO_MORE_SOURCE} = 1 ;

			return 1 ;
		     }
		  ) ;
	       }

	       $pipe->{FILTERS} = \@filters ;
	    } ;

	 }
	 elsif ( /^()   (>>?) (&)   (.*)$/x
	    ||   /^()   (&)   (>>?) (.*)$/x 
	    ||   /^(\d*)()    (>>?) (.*)$/x
	 ) {
	    croak "No command before '$_'\n" unless $cur_kid ;

	    $ops_required = $first_parse ;

            my $pipe = {
	       TYPE    => '>',
	       KFD     => length $1 ? $1 : 1,
	       TRUNC   => ! ( $2 eq '>>' || $3 eq '>>' ),
	    } ;

	    push @{$cur_kid->{OPS}}, $pipe ;
	    my $stderr_too =  $2 eq '&' || $3 eq '&' ;
	    if ( length $4 ) {
	       $pipe->{DEST} = $4 ;
	    }
	    else {
	       my @filters ;

	       if ( $ops_required ) {
		  ## unshift...shift: filters are from left to right after >
		  unshift @filters, shift @args
		     while @args > 1 && ref $args[1] ;
	       }

	       $pipe->{DEST} = shift @args ;
	       croak "'$_' missing a destination\n" if _empty $pipe->{DEST} ;

	       $self->debug(
		  'Kid ',
		  $cur_kid->{NUM},
		  "'s output fd ",
		  $pipe->{KFD},
		  ' has ',
		  scalar( @filters ),
		  ' filters.'
	       ) if @filters ;

               ## Put a filter on the end of the filter chain to pass the
	       ## output on to the CODE ref.  For SCALAR refs, the last
	       ## filter in the chain writes on the scalar itself.  See
	       ## _init_filters().
	       if ( isa( $pipe->{DEST}, 'CODE' ) ) {
		  unshift( 
		     @filters,
		     sub {
			my ( $in_ref ) = @_ ;

			return input_avail && do {
			   $pipe->{DEST}->( $$in_ref ) ;
			   $$in_ref = '' ;
			   1 ;
			}
		     }
		  ) ;
	       }
	       elsif ( isa( $pipe->{DEST}, 'GLOB' )
	          || isa( $pipe->{DEST}, 'IO::Handle' )
	       ) {
	          if ( ! defined fileno $pipe->{DEST} ) {
		     my ( $r, $w ) = $self->_pipe() ;
		     open( $pipe->{DEST}, "<&=$r" )
		        or croak "$! duping read end of pipe" ;
		     $pipe->{DEST} = $w ;
		     $pipe->{EXT_PIPE} = 1 ;
		  }
		  else {
		     $pipe->{DONT_CLOSE} = 1 ; ## this FD is not closed by us.
		  }
	       }

	       $pipe->{FILTERS} = \@filters ;
	    }
	    push @{$cur_kid->{OPS}}, {
	       TYPE => 'dup',
	       KFD1 => 1,
	       KFD2 => 2,
	    } if $stderr_too ;
	 }
	 elsif ( /^\|$/ ) {
	    croak "No command before '$_'\n" unless $cur_kid ;
	    unshift @{$cur_kid->{OPS}}, {
	       TYPE => '|',
	       KFD  => 1,
	    } ;
	    $ops_required = 0 ;
            $assumed_fd = 1 ;
	    $cur_kid = undef ;
	 }
	 elsif ( /^&$/ ) {
	    croak "No command before '$_'\n" unless $cur_kid ;
	    unshift @{$cur_kid->{OPS}}, {
	       TYPE => 'close',
	       KFD  => 0,
	    } ;
	    $ops_required = 0 ;
            $assumed_fd = 0 ;
	    $cur_kid = undef ;
	 }
	 elsif ( /^init$/ ) {
	    croak "No command before '$_'\n" unless $cur_kid ;
	    push @{$cur_kid->{OPS}}, {
	       TYPE => 'init',
	       SUB  => shift @args,
	    } ;
	 }
	 elsif ( ! $ops_required && $first_parse ) {
	    ## It's not an opcode, and no explicit opcodes have been
	    ## seen yet, so assume it's a file name.
	    unshift @args, $_ ;
	    if ( ! $assumed_fd ) {
	       $_ = "$assumed_fd<",
	    }
	    else {
	       $_ = "$assumed_fd>",
	    }
	    $self->debug( "assuming '$_'" ) ;
	    ++$assumed_fd ;
	    $first_parse = 0 ;
	    goto REPARSE ;
	 }
	 else {
	    croak join( 
	       '',
	       "Unexpected ",
	       ( ref() ? $_ : 'scalar' ),
	       " parameter in harness()\n"
	    ) ;
	 }
      } ;
      push @errs, $@ if $@ ;
   } }

   die join( '', @errs ) if @errs ;

   $self->{STATE} = _harnessed ;
   $self->timeout( $options->{timeout} ) if exists $options->{timeout} ;
   return $self ;
}


sub _open_pipes {
   my $self = shift ;

   my @errs ;

   my @close_on_fail ;

   ## When a pipe character is seen, a pipe is created.  $pipe_read_fd holds
   ## the read end of the pipe for the next process.
   my $pipe_read_fd ;

   ## Output descriptors for the last command are shared by all children.
   ## @output_fds_accum accumulates the current set of output fds.
   my @output_fds_accum ;

   ## Loop through the kids and their OPS, interpreting any that require
   ## parent-side actions.
   for my $kid ( @{$self->{KIDS}} ) {
      unless ( ref $kid->{VAL} eq 'CODE' ) {
	 $kid->{PATH} = $self->_search_path( $kid->{VAL}->[0] ) ;
      }
      if ( defined $pipe_read_fd ) {
	 unshift @{$kid->{OPS}}, {
	    TYPE => 'pipe',
	    KFD  => 0,
	    TFD  => $pipe_read_fd,
	 } ;
	 $pipe_read_fd = undef ;
      }
      @output_fds_accum = () ;
      for my $op ( @{$kid->{OPS}} ) {
	 eval {
	    if ( $op->{TYPE} eq '<' ) {
	       for my $source ( $op->{SOURCE} ) {
	       if ( $op->{EXT_PIPE} ) {
		  $op->{TFD} = $source;
		  $self->debug(
		     'kid to read ',
		     $op->{KFD},
		     ' to ',
		     $op->{TFD},
		     ' (writted to by caller)'
		  ) ;
	       }
	          elsif ( ! ref $source ) {
		     $self->debug(
			"kid ",
			$kid->{NUM},
			" to read ",
			$op->{KFD}, 
			" from '" .
			$source,
			"' (read only)"
		     );
		     croak "simulated open failure"
			if $self->{_simulate_open_failure} ;
		     $op->{TFD} = $self->_sysopen( $source, O_RDONLY ) ;
		     push @close_on_fail, $op->{TFD} ;
		  }
		  elsif ( isa( $source, 'GLOB' )
		     ||   isa( $source, 'IO::Handle' )
		  ) {
		     croak
		        "Unopened filehandle in input redirect for $op->{KFD}"
		        unless defined fileno $source ;
		     $op->{TFD} = fileno $source ;
		     $self->debug(
		        "kid ",
			$kid->{NUM},
			" to read ",
			$op->{KFD},
			" from fd ",
			$op->{TFD}
		     ) ;
		  }
		  elsif ( isa( $source, 'SCALAR' ) ) {
		     $self->debug(
		        "kid ",
			$kid->{NUM},
			" to read ",
			$op->{KFD},
			" from SCALAR"
		     ) ;

                     ( $op->{TFD}, $op->{FD} ) = $self->_pipe ;
		     push @close_on_fail, $op->{KFD}, $op->{FD} ;

		     my $s = '' ;
		     $op->{KIN_REF} = \$s ;
		  }
		  elsif ( isa( $source, 'CODE' ) ) {
		     $self->debug(
		        "kid $kid->{NUM} to read $op->{KFD} from CODE"
		     ) ;
		     ( $op->{TFD}, $op->{FD} ) = $self->_pipe ;
		     push @close_on_fail, $op->{KFD}, $op->{FD} ;
		     my $s = '' ;
		     $op->{KIN_REF} = \$s ;
		  }
		  else {
		     croak(
		        "'"
			. ref( $source )
			. "' not allowed as a source for input redirection"
		     ) ;
		  }
	       }
	       _init_filters( $op ) ;
	    }
	    elsif ( $op->{TYPE} eq '>' ) {
	       ## N> output redirection.
	       my $dest = $op->{DEST} ;
	       if ( $op->{EXT_PIPE} ) {
		  $op->{TFD} = $dest ;
		  $self->debug(
		     'kid to write ',
		     $op->{KFD},
		     ' to handle ',
		     $op->{TFD},
		     ' (read by caller)'
		  ) ;
	       }
	       elsif ( ! ref $dest ) {
		  $self->debug(
		     "kid ",
		     $kid->{NUM},
		     " to write ",
		     $op->{KFD},
		     " to '",
		     $dest,
		     "' (write only, create, ",
		     ( $op->{TRUNC} ? 'truncate' : 'append' ),
		     ")"
		  ) ;
		  croak "simulated open failure"
		     if $self->{_simulate_open_failure} ;
		  $op->{TFD} = $self->_sysopen(
		     $dest,
		     ( O_WRONLY
		     | O_CREAT 
		     | ( $op->{TRUNC} ? O_TRUNC : O_APPEND )
		     )
		  ) ;
		  push @close_on_fail, $op->{TFD} ;
	       }
	       elsif ( isa( $dest, 'GLOB' ) ) {
		  croak(
		     "Unopened filehandle in output redirect, command $kid->{NUM}"
		  ) unless defined fileno $dest ;
		  ## Turn on autoflush, mostly just to flush out
		  ## existing output.
		  my $old_fh = select( $dest ) ; $| = 1 ; select( $old_fh ) ;
		  $op->{TFD} = fileno $dest ;
		  $self->debug(
		     'kid to write ',
		     $op->{KFD},
		     ' to handle ',
		     $op->{TFD}
		  ) ;
	       }
	       elsif ( isa( $dest, 'SCALAR' ) ) {
		  $self->debug(
		     "kid ",
		     $kid->{NUM},
		     " to write $op->{KFD} to SCALAR" ) ;

		  ( $op->{FD}, $op->{TFD} ) = $self->_pipe ;
		  push @close_on_fail, $op->{FD}, $op->{TFD} ;

		  $$dest = '' if $op->{TRUNC} ;
	       }
	       elsif ( isa( $dest, 'CODE' ) ) {
		  $self->debug(
		     "kid $kid->{NUM} to write $op->{KFD} to CODE"
		  ) ;
		  ( $op->{FD}, $op->{TFD} ) = $self->_pipe ;
		  push @close_on_fail, $op->{FD}, $op->{TFD} ;
	       }
	       else {
		  croak(
		     "'"
		     . ref( $dest )
		     . "' not allowed as a sink for output redirection"
		  ) ;
	       }
	       $output_fds_accum[$op->{KFD}] = $op ;
	       _init_filters( $op ) ;
	    }
	    elsif ( $op->{TYPE} eq '|' ) {
	       $self->debug(
	          "pipelining $kid->{NUM} and "
	          . ( $kid->{NUM} + 1 )
	       ) ;
	       ( $pipe_read_fd, $op->{TFD} ) = $self->_pipe ;
	       @output_fds_accum = () ;
	    }
	    elsif ( $op->{TYPE} eq '&' ) {
	       @output_fds_accum = () ;
	    } # end if $op->{TYPE} tree
	 } # end for ( OPS }
      } ; # end eval
      push @errs, $@ if $@ ;
   } # end for ( KIDS )
   if ( @errs ) {
      for ( @close_on_fail ) {
         no strict 'refs' ;
	 $self->_close( $_ ) ;
	 $_ = undef ;
      }
      die join( '', @errs )
   }

   ## give all but the last child all of the output file descriptors
   ## These will be rewritten if the child already dup2s on to these
   ## descriptors.
   for ( my $num = 0 ; $num < $#{$self->{KIDS}} ; ++$num ) {
      for ( reverse @output_fds_accum ) {
         next unless defined $_ ;
	 $self->debug(
	    'kid ',
	    $self->{KIDS}->[$num]->{NUM},
	    ' also to write ',
	    $_->{KFD},
	    ' to ',
	    ref $_->{DEST}
	 );
	 unshift @{$self->{KIDS}->[$num]->{OPS}}, { %$_ } ;
      }
   }

   ## Open the debug pipe if we need it
   my $w_debug_fd = -1 ;
   $debug_fd = fileno STDERR ;
   $recv_debug_fd = undef ;

   if ( $self->{debug} ) {
      $self->debug( "creating debug pipe" ) ;
      ( $recv_debug_fd, $w_debug_fd ) = $self->_pipe ;
      for my $kid ( @{$self->{KIDS}} ) {
         ## We unshift this so it will be first in @files.  This makes the
	 ## kids' debugging output get printed before ours does on a given
	 ## select() call.
         unshift @{$kid->{OPS}}, {
	    TYPE => '>',
	    TFD  => $w_debug_fd,
	    KFD  => $w_debug_fd,
	    FD   => $recv_debug_fd,
	    IS_DEBUG => 1,
	 } if defined $recv_debug_fd ;
	 $kid->{DEBUG_FD} = $w_debug_fd ;
      }
   }

   ## Create the list of PIPES we need to scan and the bit vectors needed by
   ## select().  Do this first so that _cleanup can _clobber() them if an
   ## exception occurs.
   @{$self->{PIPES}} = () ;
   $self->{RIN} = '' ;
   $self->{WIN} = '' ;
   $self->{EIN} = '' ;
   ## PIN is a vec()tor that indicates who's paused.
   $self->{PIN} = '' ;
   my $num = 0 ;
   for my $kid ( @{$self->{KIDS}} ) {
      for ( @{$kid->{OPS}} ) {
	 if ( defined $_->{FD} ) {
	    $self->debug( "kid $num\[$kid->{PID}]'s $_->{KFD} is my $_->{FD}" );
	    vec( $self->{ $_->{TYPE} eq '<' ? 'WIN' : 'RIN' }, $_->{FD}, 1 ) =1;
	    if ( ! vec( $self->{EIN}, $_->{FD}, 1 ) ) {
	       vec( $self->{EIN}, $_->{FD}, 1 ) = 1 ;
	       push @{$self->{PIPES}}, $_ ;
	    }
	 }
      }
      ++$num ;
   }

   ## Put filters on the end of the filter chains to read & write the pipes.
   ## Clear pipe states
   for my $pipe ( @{$self->{PIPES}} ) {
      $pipe->{NO_MORE_SOURCE} = 0 ;
      $pipe->{PAUSED} = 0 ;
      if ( $pipe->{TYPE} eq '>' ) {
	 my $pipe_reader = sub {
	    my ( undef, $out_ref ) = @_ ;

	    return undef unless defined $pipe->{FD} ;
	    return 0 unless vec( $self->{ROUT}, $pipe->{FD}, 1 ) ;

	    vec( $self->{ROUT}, $pipe->{FD}, 1 ) = 0 ;

	    $self->debug_fd( 'reading from ', $pipe ) ;
	    my $in = $self->_read( $pipe->{FD} ) ;

	    unless ( length $in ) {
	       $self->_clobber( $pipe ) ;
	       return undef ;
	    }

	    $$out_ref .= $in ;
	    return 1 ;
	 } ;
	 ## Input filters are the last filters
	 push @{$pipe->{FILTERS}}, $pipe_reader ;
	 push @{$self->{TEMP_FILTERS}}, $pipe_reader ;
      }
      else {
	 my $pipe_writer = sub {
	    my ( $in_ref, $out_ref ) = @_ ;
	    return undef unless defined $pipe->{FD} ;
	    return 0
	       unless vec( $self->{WOUT}, $pipe->{FD}, 1 )
		  || $pipe->{PAUSED} ;

	    vec( $self->{WOUT}, $pipe->{FD}, 1 ) = 0 ;

	    if ( ! length $$in_ref ) {
	       if ( ! defined get_more_input ) {
		  $self->_clobber( $pipe ) ;
		  return undef ;
	       }
	    }

	    unless ( length $$in_ref ) {
	       unless ( $self->{PAUSED} ) {
		  $self->debug_fd( 'pausing', $pipe ) ;
		  vec( $self->{WIN}, $pipe->{FD}, 1 ) = 0 ;
		  vec( $self->{EIN}, $pipe->{FD}, 1 ) = 0 ;
		  vec( $self->{PIN}, $pipe->{FD}, 1 ) = 1 ;
		  $pipe->{PAUSED} = 1 ;
	       }
	       return 0 ;
	    }
	    $self->debug_fd( 'writing to', $pipe ) ;

	    my $c = $self->_write( $pipe->{FD}, $$in_ref ) ;
	    substr( $$in_ref, 0, $c, '' ) ;
	    return 1 ;
	 } ;
	 ## Output filters are the first filters
	 unshift @{$pipe->{FILTERS}}, $pipe_writer ;
	 push    @{$self->{TEMP_FILTERS}}, $pipe_writer ;
      }
   }
}


sub _dup2_gently {
   my $self = shift ;
   my ( $files, $fd1, $fd2 ) = @_ ;
   ## Moves TFDs that are using the destination fd out of the
   ## way before calling _dup2
   for ( @$files ) {
      next unless defined $_->{TFD} ;
      $_->{TFD} = $self->_dup( $_->{TFD} ) if $_->{TFD} == $fd2 ;
   }
   $self->_dup2( $fd1, $fd2 ) ;
}


sub _do_kid {
   my $self = shift ;
   my ( $kid ) = @_ ;

   $debug_fd = $kid->{DEBUG_FD} ;
   ## close parent FD's first so they're out of the way.
   ## Don't close STDIN, STDOUT, STDERR: they should be inherited or
   ## overwritten below.
   my @needed = ( 1, 1, 1 ) ;
   for ( @{$kid->{OPS}} ) {
      $needed[ $_->{TFD} ] = 1 if defined $_->{TFD} ;
   }
   my @closed ;
   for ( @{$self->{KIDS}} ) {
      for ( @{$_->{OPS}} ) {
         for ( $_->{FD}, ( $_ != $kid ? $_->{TFD} : () ) ) {
	    if ( defined $_ && ! $closed[$_] && ! $needed[$_] ) {
	       $self->_close( $_ ) ;
	       $closed[$_] = 1 ;
	       $_ = undef ;
	    }
         }
      }
   }

   for ( @{$kid->{OPS}} ) {
      if ( defined $_->{TFD} ) {
	 unless ( $_->{TFD} == $_->{KFD} ) {
	    $self->_dup2_gently( $kid->{OPS}, $_->{TFD}, $_->{KFD} ) ;
	    $self->_close( $_->{TFD} ) ;
	    $closed[$_->{TFD}] = 1 ;
	    $_->{TFD} = undef ;
	 }
      }
      elsif ( $_->{TYPE} eq 'dup' ) {
	 $self->_dup2_gently( $kid->{OPS}, $_->{KFD1}, $_->{KFD2} )
	    unless $_->{KFD1} == $_->{KFD2} ;
      }
      elsif ( $_->{TYPE} eq 'close' ) {
         for ( $_->{KFD} ) {
	    if ( ! $closed[$_] ) {
	       $self->_close( $_ ) ;
	       $closed[$_] = 1 ;
	       $_ = undef ;
	    }
	 }
      }
      elsif ( $_->{TYPE} eq 'init' ) {
         $_->{SUB}->() ;
      }
   }

   if ( ref $kid->{VAL} eq 'CODE' ) {
      $kid->{VAL}->() ;
      exit 0 ;
   }
   else {
$| = 1 ;
      $self->debug( 'execing ', $kid->{PATH} ) ;
      POSIX::close( $debug_fd ) if defined $debug_fd ;
      $self->_exec( $kid->{PATH}, @{$kid->{VAL}}[1..$#{$kid->{VAL}}] ) ;
      croak "exec of $kid->{PATH} failed $!" ;
   }
}


=item start

   $h = start \@cmd, \$in, \$out, ... ;
   $h = start \@cmd, '<', \$in, '|', \@cmd2, ... ;

start() accepts a harness or harness specification and returns a harness
after building all of the pipes and launching (via fork()/exec(), or, maybe
someday, spawn()) all the child processes.  It does not send or receive any
data on the pipes, see pump() and finish() for that.

You may call harness() and then pass it's result to start() if you like,
but you only need to if it helps you structure or tune your application.
If you do call harness(), you may skip start() and proceed directly to
pump.

start() also starts the timeout timer if one has been defined with
timeout().

=cut

sub start {
   my $options = @_ > 1 && ref $_[-1] eq 'HASH' ? pop : {} ;
   $options->{debug} = $debug || 0 unless defined $options->{debug} ;

   my $self ;
   if ( @_ == 1 && isa( $_[0], __PACKAGE__ ) ) {
      $self = shift ;
      $self->{$_} = $options->{$_} for keys %$options ;
   }
   else {
      $self = harness( @_, $options ) ;
   }

   $self->debug( "** starting" ) ;

   ## Assume we're not being called from &run.  It will correct our
   ## assumption if need be.  This affects whether &_select_loop clears
   ## input queues to '' when they're empty.
   $self->{clear_ins} = 1 ;

   my @errs ;

   eval {
      $self->_open_pipes ;
   } ;
   push @errs, $@ if $@ ;

   if ( ! @errs ) {
      for my $kid ( @{$self->{KIDS}} ) {
	 eval {
	    croak "simulated failure of fork" if $self->{_simulate_fork_failure} ;
	    unless ( $^O =~ /^(?:os2|MSWin32)$/ ) {
	       $kid->{PID} = fork() ;
	       $self->_do_kid( $kid ) unless $kid->{PID} ;
	       croak "$! during fork" unless defined $kid->{PID} ;
	       $self->debug( "fork() = $kid->{PID}" ) ;
	    }
	    else {
	       $self->debug( 
		  'spawning ',
		  join(
		     ' ',
		     map {
			"'$_'"
		     } ( $kid->{PATH}, @{$kid->{VAL}}[1..$#{$kid->{VAL}}] )
		  )
	       ) ;
	       require IPC::Open3 ;
	       require IO::Handle ;
	       my @close_in_kid ;
	       my $op ;

	       my @needed = (1,1,1) ;
	       for $op ( @{$kid->{OPS}} ) {
		  $needed[$op->{TFD}] = 1 if defined $op->{TFD}
	       }

	       for my $k ( @{$self->{KIDS}} ) {
		  for $op ( @{$k->{OPS}} ) {
		     my $mode = $op->{TYPE} eq '>' ? 'w' : 'r' ;
		     for ( $op->{FD}, ( $k != $kid ? $_->{TFD} : () ) ) {
			push @close_in_kid, IO::Handle->new_from_fd( $_, $mode )
			   if defined $_ && ! $needed[ $_ ] ;
		     }
		  }
	       }

	       my @handle_map ;
	       for $op ( @{$kid->{OPS}} ) {
		  my $mode = $op->{TYPE} eq '>' ? 'w' : 'r' ;
		  push @handle_map, {
		     mode    => $mode,
		     open_as => IO::Handle->new_from_fd( $op->{KFD}, $mode ),
		     handle  => IO::Handle->new_from_fd( $op->{TFD}, $mode ),
		  } if defined $op->{TFD} ;
	       }
	       $kid->{PID} = IPC::Open3::spawn_with_handles(
		  \@handle_map,
		  \@close_in_kid,
		  $kid->{PATH},
		  @{$kid->{VAL}}[1..$#{$kid->{VAL}}]
	       ) ;
	       $self->debug( "spawn_with_handles() = $kid->{PID}" ) ;
	    }
	 } ;
	 push @errs, $@ if $@ ;
      }
   }

   ## Close all those temporary filehandles that the kids needed.
   my @closed ;
   for my $kid ( @{$self->{KIDS}} ) {
      for ( @{$kid->{OPS}} ) {
	 eval {
	    if ( defined $_->{TFD}
	       && ! $_->{DONT_CLOSE}
	       && ! $closed[$_->{TFD}]
	    ) {
	       $self->_close( $_->{TFD} ) ;
	       $closed[$_->{TFD}] = 1 ;
	       $_->{TFD} = undef ;
	    }
	 } ;
	 push @errs, $@ if $@ ;
      }
   }

   if ( @errs ) {
      eval { $self->_cleanup } ;
      warn $@ if $@ ;
      die join( '', @errs ) ;
   }

   $self->{STATE} = _started ;
   $self->_calc_timeout_end ;
   return $self ;
}


sub _clobber {
   my $self = shift ;
   my ( $file ) = @_ ;
   $self->debug_fd( "closing", $file ) ;
   my $doomed = $file->{FD} ;
   vec( $self->{RIN}, $doomed, 1 ) = 0 ;
   vec( $self->{WIN}, $doomed, 1 ) = 0 ;
   vec( $self->{EIN}, $doomed, 1 ) = 0 ;
   $self->_close( $doomed ) ;
   @{$self->{PIPES}} = grep { $_->{FD} ne $doomed } @{$self->{PIPES}} ;
   $file->{FD} = undef ;
}


sub _select_loop {
   my IPC::Run $self = shift ;

   my $nfound ;
   my $timeout = $self->{non_blocking} ? 0 : undef ;

   my $io_occurred ;

   croak "process ended prematurely" unless @{$self->{PIPES}} ;

SELECT:
   while ( @{$self->{PIPES}} ) {
      last if $io_occurred && $self->{break_on_io} ;

      if ( defined $self->{TIMEOUT_END} ) {
         my $now = time ;
	 die "process timed out" if $now > $self->{TIMEOUT_END} ;
	 my $delta = $self->{TIMEOUT_END} - $now ;
	 $timeout = $delta if ! defined $timeout || $timeout > $delta ;
      }

      ##
      ## See if we can unpause any input channels
      ##
      my $paused = 0 ;

      for my $file ( @{$self->{PIPES}} ) {
         next unless $file->{PAUSED} && $file->{TYPE} eq '<' ;

	 my $did ;
	 1 while ( $did = _do_filters( $file ) ) ;
	 if ( defined $file->{FD} && ! defined( $did ) || $did ) {
	    $self->debug_fd( "unpausing", $file ) ;
	    $file->{PAUSED} = 0 ;
	    vec( $self->{WIN}, $file->{FD}, 1 ) = 1 ;
	    vec( $self->{EIN}, $file->{FD}, 1 ) = 1 ;
	    vec( $self->{PIN}, $file->{FD}, 1 ) = 0 ;
	 }
	 else {
	    ++$paused ;
	 }
      }

      if ( $self->{debug} > 1 ) {
         my $map = join(
	    '',
	    map {
	       my @out ;
	       push @out, 'r', if vec( $self->{RIN}, $_, 1 ) ;
	       push @out, 'w', if vec( $self->{WIN}, $_, 1 ) ;
	       push @out, 'p', if vec( $self->{PIN}, $_, 1 ) ;
	       ! @out
		  ? '-'
		  : @out == 1
		     ? $out[0]
		     : ( '(', @out, ')' ) ;
	    } (0..128)
	 ) ;
	 $map =~ s/((?:[a-z-]|\([^\)]*\)){12,}?)-*$/$1/ ;
	 $self->debug( 'selecting ', $map ) ;
      }

      my $nfound = select(
         $self->{ROUT} = $self->{RIN},
	 $self->{WOUT} = $self->{WIN},
	 $self->{EOUT} = $self->{EIN},
	 $timeout 
      ) ;

      last if ! $nfound && $self->{non_blocking} ;


      croak "$! in select" if $nfound < 0 ;
      if ( $self->{debug} ) {
         my $map = join(
	    '',
	    map {
	       my @out ;
	       push @out, 'r', if vec( $self->{ROUT}, $_, 1 ) ;
	       push @out, 'w', if vec( $self->{WOUT}, $_, 1 ) ;
	       push @out, 'e', if vec( $self->{EOUT}, $_, 1 ) ;
	       ! @out
		  ? '-'
		  : @out == 1
		     ? $out[0]
		     : ( '(', @out, ')' ) ;
	    } (0..128)
	 ) ;
	 $map =~ s/((?:[a-z-]|\([^\)]*\)){12,}?)-*$/$1/ ;
	 $self->debug( "selected  $map" ) ;
      }

      ## Need to copy since _clobber alters @{$self->{PIPES}}.
      ## TODO: Rethink _clobber().  Rething $file->{PAUSED}, too.
      my @files = @{$self->{PIPES}} ;
   FILE:
      for my $file ( @files ) {
	 if ( vec( $self->{ROUT}, $file->{FD}, 1 ) ) {
	    if ( $file->{IS_DEBUG} ) {
	       ## When receiving debug messages, use POSIX.  We don't want to 
	       ## have _read and _write log things for us.
	       my $in ;
	       my $c = POSIX::read( $file->{FD}, $in, 100000 ) ;
	       if( defined $c && $c > 0 ) {
		  POSIX::write( $debug_fd, $in, length $in ) ;
	       }
	       else {
	          $self->_clobber( $file ) ;
	       }

	       next FILE ;
	    }

	    $self->debug_fd( "filtering data from", $file ) ;
	    $io_occurred = 1 if _do_filters( $file ) ;

	    next FILE unless defined $file->{FD} ;
	 }

	 if ( vec( $self->{WOUT}, $file->{FD}, 1 ) ) {
	    $self->debug_fd( "filtering data to", $file ) ;
	    $io_occurred = 1 if _do_filters( $file ) ;

	    next FILE unless defined $file->{FD} ;
	 }

	 if ( vec( $self->{EOUT}, $file->{FD}, 1 ) ) {
	    croak "Exception on file $file->{FD}" ;
	 }
      }
   }

   return scalar( @{$self->{PIPES}} ) ;
}


sub _cleanup {
   my $self = shift ;
   my $num = 0 ;
   $self->debug( "cleaning up" ) ;
   ## _clobber modified PIPES
   $self->_clobber( $self->{PIPES}->[0] ) while @{$self->{PIPES}} ;
   for my $kid ( @{$self->{KIDS}} ) {
      if ( ! length $kid->{PID} ) {
	 $self->debug( 'never ran child ', $num++, ", can't reap" ) ;
	 for my $op ( @{$kid->{OPS}} ) {
	    $self->_close( $op->{TFD} ) if defined $op->{TFD} ;
	 }
      }
      else {
	 $self->debug( 'reaping child ', $num++, ' (pid ', $kid->{PID}, ')' ) ;
	 my $pid = waitpid $kid->{PID}, 0 ;
	 $kid->{RESULT} = $? ;
	 $self->debug( 'reaped ', $pid, ', $?=', $kid->{RESULT} ) ;
      }

      if ( defined $kid->{DEBUG_FD} ) {
         @{$kid->{OPS}} = grep {
	    $_->{KFD} != $kid->{DEBUG_FD}
	 } @{$kid->{OPS}} ;
	 $kid->{DEBUG_FD} = undef ;
      }

      for my $op ( @{$kid->{OPS}} ) {
	 @{$op->{FILTERS}} = grep {
	    my $filter = $_ ;
	    ! grep {
	       $filter == $_ ;
	    } @{$self->{TEMP_FILTERS}} ;
	 } @{$op->{FILTERS}} ;
      }

   }
   $self->{STATE} = _harnessed ;
   @{$self->{TEMP_FILTERS}} = () ;
}


=item pump

   pump $h ;
   $h->pump ;

Pump accepts a single parameter harness.  It blocks until it delivers some
input or recieves some output.  It returns TRUE if there
is still input or output to be done, FALSE otherwise.

pump() will automatically call start() if need be, so you may call harness()
then proceed to pump() if that helps you structure your application.

Calling pump() when there is no more i/o do be done causes a "process ended
prematurely" exception to be thrown.  This allows fairly simple scripting
of external applications without having to add lots of error handling code
at each step of the script:

   $h = harness \@smbclient, \$in_q, \$out_q, $err_q ;

   $in_q = "cd /foo\n" ;
   $h->pump until $out_q =~ /^smb.*> \Z/m ;
   die "error cding to /foo:\n$out_q" if $out_q =~ "ERR" ;
   $out_q = '' ;

   $in_q = "mget *\n" ;
   $h->pump until $out_q =~ /^smb.*> \Z/m ;
   die "error retrieving files:\n$out_q" if $out_q =~ "ERR" ;

   $h->finish ;

   warn $err_q if $err_q ;


=cut


sub pump {
   die "pump() takes only a a single harness as a parameter"
      unless @_ == 1 && isa( $_[0], __PACKAGE__ ) ;

   my IPC::Run $self = shift ;

   $self->{debug} = $debug unless defined $self->{debug} ;
   $self->debug( "** pumping" ) ;

   my $r = eval {
      $self->start if $self->{STATE} < _started ;
      $self->{no_close_ins} = 1 ;
      $self->{break_on_io} = 1 ;
      $self->_select_loop ;
   } ;
   if ( $@ ) {
      my $x = $@ ;
      $self->debug( $x ) if $x ;
      eval { $self->_cleanup } ;
      warn $@ if $@ ;
      die $x ;
   }
   return $r ;
}


=item pump_nb

   pump_nb $h ;
   $h->pump_nb ;

"pump() non-blocking", pumps if anything's ready to be pumped, returns
immediately otherwise.  This is useful if you're doing some long-running
task in the foreground, but don't want to starve any child processes.

=cut

sub pump_nb {
   my IPC::Run $self = shift ;

   $self->{non_blocking} = 1 ;
   my $r = eval { $self->pump } ;
   $self->{non_blocking} = 0 ;
   die $@ if $2 ;
   return $r ;
}

=cut


sub pump {
   die "pump() takes only a a single harness as a parameter"
      unless @_ == 1 && isa( $_[0], __PACKAGE__ ) ;

   my $self = shift ;

   $self->{debug} = $debug unless defined $self->{debug} ;
   $self->debug( "** pumping" ) ;

   my $r = eval {
      $self->start if $self->{STATE} < _started ;
      $self->{non_blocking} = 0 ;
      $self->{no_close_ins} = 1 ;
      $self->{break_on_io} = 1 ;
      $self->_select_loop ;
   } ;
   if ( $@ ) {
      my $x = $@ ;
      $self->debug( $x ) if $x ;
      eval { $self->_cleanup } ;
      warn $@ if $@ ;
      die $x ;
   }
   return $r ;
}



=item pumpable

Returns TRUE if calling pump() won't throw an immediate "process ended
prematurely" exception.  Reflects the value returned from the last call
to pump().

=cut

sub pumpable {
   my IPC::Run $self = shift ;
   return scalar @{$self->{PIPES}} ;
}


=item finish

This must be called after the last start() or pump() call for a harness,
or your system will accumulate defunct processes and you may "leak"
file descriptors.

finish() returns TRUE if all children returned 0, and FALSE otherwise (this
is like run(), and the opposite of system()).

Once a harness has been finished, it may be run() or start()ed again,
including by pump()s auto-start.

=cut


sub finish {
   my $self = shift ;
   my $options = @_ && ref $_[-1] eq 'HASH' ? pop : {} ;

   $self->debug( "** finishing" ) ;

   eval {
      $self->{non_blocking} = 0 ;
      $self->{no_close_ins} = 0 ;
      $self->{break_on_io} = 0 ;
      # We don't alter $self->{clear_ins}
      $self->_select_loop( $options ) if @{$self->{PIPES}} ;
   } ;
   my $x = $@ ;
   $self->debug( $x ) if $x ;
   eval { $self->_cleanup } ;
   warn $@ if $@ ;
   die $x if $x ;

   return ! grep { $_->{RESULT} } @{$self->{KIDS}} ;
}

##
## Filter Scaffolding
##
my $filter_op  ;        ## The op running a filter chain right now
my $filter_num ;        ## Which filter is being run right now.

sub _init_filters {
   my ( $op ) = @_ ;

   if ( ! $op->{FILTERS} ) {
      $op->{FBUFS} = undef ;
      return ;
   }

   @{$op->{FBUFS}} = map {
      my $s = "" ;
      \$s ;
   } ( @{$op->{FILTERS}}, '' ) ;

   $op->{FBUFS}->[0] = $op->{DEST}
      if $op->{DEST} && ref $op->{DEST} eq 'SCALAR' ;
   push @{$op->{FBUFS}}, $op->{SOURCE} ;
}


sub _do_filters {
   my ( $saved_op, $saved_num ) = ( $filter_op, $filter_num ) ;
   ( $filter_op ) = @_ ;
   confess "\$filter_op is a " . ref $filter_op unless ref $filter_op eq 'HASH';
   $filter_num = -1 ;
   my $r = eval { get_more_input() ; } ;
   ( $filter_op, $filter_num ) = ( $saved_op, $saved_num ) ;
   die $@ if $@ ;
   return $r ;
}


##
## A few filters and filter constructors
##

=back

=head1 FILTERS

These filters are used to modify input our output between a child
process and a scalar or subroutine endpoint.

=over

=item new_chunker

This breaks a stream of data in to chunks, based on an optional
scalar or regular expression parameter.  The default is the Perl
input record separator in $/, which is a newline be default.

   run( \@cmd,
      '>', new_chunker, \&lines_handler,
   ) ;

   run( \@cmd,
      '>', new_chunker( "\r\n" ), \&lines_handler,
   ) ;

Because this uses $/ by default, you should always pass in a parameter
if you are worried about other code (modules, etc) modifying $/.

If this filter is last in a filter chain that dumps in to a scalar,
the scalar must be set to '' before a new chunk will be written to it.

As an example of how a filter like this can be written, here's a
chunker that splits on newlines:

   sub line_splitter {
      my ( $in_ref, $out_ref ) = @_ ;

      return 0 if length $$out_ref ;

      return input_avail && do {
	 while (1) {
	    if ( $$in_ref =~ s/\A(.*?\n)// ) {
	       $$out_ref .= $1 ;
	       return 1 ;
	    }
            my $hmm = get_more_input ;
	    unless ( defined $hmm ) {
	       $$out_ref = $$in_ref ;
	       $$in_ref = '' ;
	       return length $$out_ref ? 1 : 0 ;
	    }
	    return 0 if $hmm eq 0 ;
	 }
      }
   } ;

=cut

sub new_chunker(;$) {
   my ( $re ) = @_ ;
   $re = $/ if _empty $re ;
   $re = quotemeta( $re ) unless ref $re eq 'Regexp' ;
   $re = qr/\A(.*?$re)/s ;

   return sub {
      my ( $in_ref, $out_ref ) = @_ ;

      return 0 if length $$out_ref ;

      return input_avail && do {
	 while (1) {
	    if ( $$in_ref =~ s/$re// ) {
	       $$out_ref .= $1 ;
	       return 1 ;
	    }
            my $hmm = get_more_input ;
	    unless ( defined $hmm ) {
	       $$out_ref = $$in_ref ;
	       $$in_ref = '' ;
	       return length $$out_ref ? 1 : 0 ;
	    }
	    return 0 if $hmm eq 0 ;
	 }
      }
   } ;
}


=item new_appender

This appends a fixed string to each chunk of data read from the source
scalar or sub.  This might be useful if you're writing commands to a
child process that always must end in a fixed string, like "\n":

   run( \@cmd,
      '<', new_appender( "\n" ), \&commands,
   ) ;

Here's a typical filter sub that might be created by new_appender():

   sub newline_appender {
      my ( $in_ref, $out_ref ) = @_ ;

      return input_avail && do {
	 $$out_ref = join( '', $$out_ref, $$in_ref, "\n" ) ;
	 $$in_ref = '' ;
	 1 ;
      }
   } ;

=cut

sub new_appender($) {
   my ( $suffix ) = @_ ;
   croak "\$suffix undefined" unless defined $suffix ;

   return sub {
      my ( $in_ref, $out_ref ) = @_ ;

      return input_avail && do {
	 $$out_ref = join( '', $$out_ref, $$in_ref, $suffix ) ;
	 $$in_ref = '' ;
	 1 ;
      }
   } ;
}


sub new_string_source {
   my $ref ;
   if ( @_ > 1 ) {
      $ref = [ @_ ],
   }
   else {
      $ref = shift ;
   }

   return ref $ref eq 'SCALAR'
      ? sub {
	 my ( $in_ref, $out_ref ) = @_ ;

	 return defined $$ref
	    ? do {
	       $$out_ref .= $$ref ;
	       my $r = length $$ref ? 1 : 0 ;
	       $$ref = undef ;
	       $r ;
	    }
	    : undef
      }
      : sub {
	 my ( $in_ref, $out_ref ) = @_ ;

	 return @$ref
	    ? do {
	       my $s = shift @$ref ;
	       $$out_ref .= $s ;
	       length $s ? 1 : 0 ;
	    }
	    : undef ;
      }
}


sub new_string_sink {
   my ( $string_ref ) = @_ ;

   return sub {
      my ( $in_ref, $out_ref ) = @_ ;

      return input_avail && do {
	 $$string_ref .= $$in_ref ;
	 $$in_ref = '' ;
	 1 ;
      }
   } ;
}


=item timeout

This function defines a time interval, starting from when start() is
called, or when timeout() is called.  If all processes have not finished
by the end of the timeout period, then a "process timed out" exception
is thrown.

The time interval may be passed in seconds, or as an end time in
"HH:MM:SS" format (any non-digit other than '.' may be used as
spacing and puctuation).  This is probably best shown by example:

   $h->timeout( $val ) ;

   $val                     Effect
   ======================== =====================================
   undef                    Timeout timer disabled
   ''                       Almost immediate timeout
   0                        Almost immediate timeout
   0.000001                 timeout > 0.0000001 seconds
   30                       timeout > 30 seconds
   30.0000001               timeout > 30 seconds
   10:30                    timeout > 10 minutes, 30 seconds

Timeouts are currently evaluated with a 1 second resolution, though
this may change in the future.  This means that setting
timeout($h,1) will cause a pokey child to be aborted sometime after
one second has elapsed and typically before two seconds have elapsed.

This sub does not check whether or not the timeout has expired already.

Returns the number of seconds set as the timeout (this does not change
as time passes, unless you call timeout( val ) again).

The timeout does not include the time needed to fork() or spawn()
the child processes, though some setup time for the child processes can
included.  It also does not include the length of time it takes for
the children to exit after they've closed all their pipes to the
parent process.

=cut

sub timeout {
   my IPC::Run $self = shift ;

   if ( @_ ) {
      ( $self->{TIMEOUT} ) = @_ ;
      $self->{TIMEOUT_END} = undef ;
      if ( defined $self->{TIMEOUT} ) {
	 if ( $self->{TIMEOUT} =~ /[^\d.]/ ) {
	    my @f = split( /[^\d\.]+/i, $self->{TIMEOUT} ) ;
	    unshift @f, 0 while @f < 3 ;
	    $self->{TIMEOUT} = (($f[0]*60)+$f[1])*60+$f[2] ;
	 }
	 elsif ( $self->{TIMEOUT} =~ /^(\d*)(?:\.(\d*))/ ) {
	    $self->{TIMEOUT} = $1 + 1 ;
	 }
	 $self->_calc_timeout_end if $self->{STATE} >= _started ;
      }
   }
   return $self->{TIMEOUT} ;
}


sub _calc_timeout_end {
   my IPC::Run $self = shift ;

   $self->{TIMEOUT_END} = defined $self->{TIMEOUT} 
      ? time + $self->{TIMEOUT}
      : undef ;
}


=back

=head1 FILTER IMPLEMENTATION FUNCTIONS

These functions are for use from within filters.

=over

=item input_avail

Returns TRUE if input is available.  If none is available, then 
&get_more_input is called and it's result returned.

This is usually used in preference to &get_more_input so that the
calling filter removes all data from the $in_ref before more data
gets read in to $in_ref.

C<get_more_input> is usually used as part of a return expression:

   return input_avail && do {
      ## process the input just gotten
      1 ;
   } ;

This technique allows get_more_input to return the undef or 0 that a
filter normally returns when there's no input to process.  If a filter
stores intermediate values, however, it will need to react to an
undef:

   my $got = input_avail ;
   if ( ! defined $got ) {
      ## No more input ever, flush internal buffers to $out_ref
   }
   return $got unless $got ;
   ## Got some input, move as much as need be
   return 1 if $added_to_out_ref ;

=cut

sub input_avail() {
   confess "Undefined FBUF ref for $filter_num+1"
      unless defined $filter_op->{FBUFS}->[$filter_num+1] ;
   length ${$filter_op->{FBUFS}->[$filter_num+1]} || get_more_input ;
}


=item get_more_input

This is used to fetch more input in to the input variable.  It returns
undef if there will never be any more input, 0 if there is none now,
but there might be in the future, and TRUE if more input was gotten.

C<get_more_input> is usually used as part of a return expression,
see L</input_avail> for more information.

=cut

##
## Filter implementation interface
##
sub get_more_input() {
   ++$filter_num ;
   my $r = eval {
      confess "get_more_input() called and no more filters in chain"
         unless defined $filter_op->{FILTERS}->[$filter_num] ;
      $filter_op->{FILTERS}->[$filter_num]->(
         $filter_op->{FBUFS}->[$filter_num+1],
         $filter_op->{FBUFS}->[$filter_num],
      ) ; # if defined ${$filter_op->{FBUFS}->[$filter_num+1]} ;
   } ;
   --$filter_num ;
   die $@ if $@ ;
   return $r ;
}

=item filter_tests

   my @tests = filter_tests( "foo", "in", "out", \&filter ) ;
   $_->() for ( @tests ) ;

This creates a list of test subs that can be used to test most filters
for basic functionality.  The first parameter is the name of the
filter to be tested, the second is sample input, the third is the
test(s) to apply to the output(s), and the rest of the parameters are
the filters to be linked and tested.

If the filter chain is to be fed multiple inputs in sequence, the second
parameter should be a reference to an array of thos inputs:

   my @tests = filter_tests( "foo", [qw(1 2 3)], "123", \&filter ) ;

If the filter chain should produce a sequence of outputs, then the
thrid parameter should be a reference to an array of those outputs:

   my @tests = filter_tests(
      "foo",
      "1\n\2\n",
      [ qr/^1$/, qr/^2$/ ],
      new_chunker
   ) ;

See t/run.t and t/filter.t for an example of this in practice.

=cut

##
## Filter testing routines
##
sub filter_tests($;@) {
   my ( $name, $in, $exp, @filters ) = @_ ;

   my @in  = ref $in  eq 'ARRAY' ? @$in  : ( $in  ) ;
   my @exp = ref $exp eq 'ARRAY' ? @$exp : ( $exp ) ;

   require Test ;
   *ok = \&Test::ok ;

   my $op ;
   my $output ;
   my @input ;
   my $in_count = 0 ;

   my @out ;

   return (
      sub {
	 $op = {
	    FILTERS => [
	       new_string_sink( \$output ),
	       @filters,
	       new_string_source( \@input ),
	    ],
	 } ;
	 _init_filters( $op ) ;
	 @input = () ;
	 $output = '' ;
	 ok(
	    ! defined _do_filters( $op ),
	    1,
	    "$name didn't pass undef (EOF) through"
	 ) ;
      },

      ## See if correctly does nothing on 0, (please try again)
      sub {
         _init_filters( $op ) ;
	 $output = '' ;
	 @input = ( '' ) ;
	 ok(
	    _do_filters( $op ),
	    0,
	    "$name didn't return 0 (please try again) when given a 0"
	 ) ;
      },

      sub {
	 @input = ( '' ) ;
	 ok(
	    _do_filters( $op ),
	    0,
	    "$name didn't return 0 (please try again) when given a second 0"
	 ) ;
      },

      sub {
	 for (1..100) {
	    last unless defined _do_filters( $op ) ;
	 }
	 ok(
	    ! defined _do_filters( $op ),
	    1,
	    "$name didn't return undef (EOF) after two 0s and an undef"
	 ) ;
      },

      ## See if it can take @in and make @out
      sub {
         _init_filters( $op ) ;
	 $output = '' ;
	 @input = @in ;
	 while ( defined _do_filters( $op ) && @input ) {
	    if ( length $output ) {
	       push @out, $output ;
	       $output = '' ;
	    }
	 }
	 if ( length $output ) {
	    push @out, $output ;
	    $output = '' ;
	 }
	 ok(
	    scalar @input,
	    0,
	    "$name didn't consume it's input"
	 ) ;
      },

      sub {
	 for (1..100) {
	    last unless defined _do_filters( $op ) ;
	    if ( length $output ) {
	       push @out, $output ;
	       $output = '' ;
	    }
	 }
	 ok(
	    ! defined _do_filters( $op ),
	    1,
	    "$name didn't return undef (EOF), tried  100 times"
	 ) ;
      },

      sub {
	 ok(
	    join( ', ', map "'$_'", @out ),
	    join( ', ', map "'$_'", @exp ),
	    $name
	 )
      },

   ) ;
}


=back

=head1 TODO

These will be addressed as needed and as time allows.

Stall timeout.

Expose a list of child process objects.  When I do this,
each child process is likely to be blessed into IPC::Run.

$kid->abort(), $kid->kill(), $kid->signal( $num_or_name ).

Allow retrieving each kid's result code.

Currently, pump() and run() only work on systems where select() works on the
filehandles returned by pipe().  This does *not* include Win32.  I'd
like to rectify that, suggestions and patches welcome.

Likewise start() only fully works on fork()/exec() machines (well, just
fork() if you only ever pass perl subs as subprocesses).  There's
some scaffolding for calling Open3::spawn_with_handles(), but that's
not that useful with limited select().

Support for C<\@sub_cmd> as an argument to a command which
gets replaced with /dev/fd or the name of a temporary file containing foo's
output.  This is like <(sub_cmd ...) found in bash and csh (IIRC).

Allow multiple harnesses to be combined as independant sets of processes
in to one 'meta-harness'.

Allow a harness to be passed in place of an \@cmd.

Ability to add external file descriptors w/ filter chains and endpoints.

Ability to add timeouts and timing generators (i.e. repeating timeouts).

High resolution timeouts.

=head1 LIMITATIONS

No support for ';', '&&', '||', '{ ... }', etc: use perl's, since run()
returns TRUE when the command exits with a 0 result code.

Does not provide shell-like string interpolation.

No support for C<cd>, C<setenv>, or C<export>: use %ENV in an init => sub{ }:

   run(
      \cmd,
         ...
	 init => sub {
	    chdir $dir or die $! ;
	    $ENV{FOO}='BAR'
	 }
   ) ;

Timeout calculation does not allow absolute times, or specification of
days, months, etc.

=item KNOWN BUGS

run() actually does try to modify input scalars if they don't get
written in a single call to POSIX::write().

=head1 INSPIRATION

Well, select() and waitpid() badly needed wrapping, and open3() isn't
open-minded enough for me.

API inspired by a message Russ Allbery sent to perl5-porters, which
included:

   I've thought for some time that it would be
   nice to have a module that could handle full Bourne shell pipe syntax
   internally, with fork and exec, without ever invoking a shell.  Something
   that you could give things like:

   pipeopen (PIPE, [ qw/cat file/ ], '|', [ 'analyze', @args ], '>&3');

Message ylln51p2b6.fsf@windlord.stanford.edu, on 2000/02/04.

=head1 AUTHOR

Barrie Slaymaker <barries@slaysys.com>, with numerous suggestions by p5p.

=cut

1 ;
