package IPC::Run::Win32Helper ;

=head1 NAME

IPC::Run::Win32Helper - helper routines for IPC::Run on Win32 platforms.

=head1 SYNOPSIS

use IPC::Run::Win32Helper ;   # Exports all by default

=head1 DESCRIPTION

IPC::Run needs to use sockets to redirect subprocess I/O so that the select() loop
will work on Win32. This seems to only work on WinNT and Win2K at this time, not
sure if it will ever work on Win95 or Win98. If you have experience in this area, please
contact me at barries@slaysys.com, thanks!.

=cut

@ISA = qw( Exporter ) ;

@EXPORT = qw(
   win32_fake_pipe
   win32_spawn
   win32_parse_cmd_line
   _pump
   _dont_inherit
) ;

use strict ;
use Carp ;
use IO::Handle ;
#use IPC::Open3 ();
use Socket ;
require POSIX ;

## Work around missing prototypes in old Socket.pm versions
sub Socket::IPPROTO_TCP() ;
sub Socket::TCP_NODELAY() ;

use Socket qw( IPPROTO_TCP TCP_NODELAY ) ;
use Symbol ;
use Text::ParseWords ;
use Win32::Process ;
use Win32API::File qw(
   GetOsFHandle
   OsFHandleOpenFd
   FdGetOsFHandle
   SetHandleInformation
   HANDLE_FLAG_INHERIT
   INVALID_HANDLE_VALUE
) ;



BEGIN {
   ## Force AUTOLOADED constants to be, well, constant by getting them
   ## to AUTOLOAD before compilation continues.  Sigh.
   SOL_SOCKET;
   SO_REUSEADDR;
   IPPROTO_TCP;
   TCP_NODELAY;
   HANDLE_FLAG_INHERIT;
   INVALID_HANDLE_VALUE;
}


## We need to prototype these so they don't conflict.
sub _debug ;
sub _debugging_details() ;
sub _debugging_gory_details() ;

## Sometimes, Win32Helper.pm is loaded first (in the pumpers), and it
## loads IPC::Run.  We need to forward declare these to prevent warnings
## in that case.

sub IPC::Run::_debugging_details() ;
sub IPC::Run::_debugging_gory_details() ;

*_debug                  = \&IPC::Run::_debug ;
*_debugging_details      = \&IPC::Run::_debugging_details ;
*_debugging_gory_details = \&IPC::Run::_debugging_gory_details ;

## Takes an fd or a GLOB ref, never never never a Win32 handle.
sub _dont_inherit {
   for ( @_ ) {
      next unless defined $_ ;
      my $fd = $_ ;
      $fd = fileno $fd if ref $fd ;
      _debug "disabling inheritance of ", $fd if _debugging_details ;
      my $osfh = FdGetOsFHandle $fd ;
      croak $^E if ! defined $osfh || $osfh == INVALID_HANDLE_VALUE ;

      SetHandleInformation( $osfh, HANDLE_FLAG_INHERIT, 0 ) ;
   }
}

#sub _inherit {
#   for ( @_ ) {
#      next unless defined $_ ;
#      my $osfh = GetOsFHandle $_ ;
#      croak $^E if ! defined $osfh || $osfh == INVALID_HANDLE_VALUE ;
#      SetHandleInformation( $osfh, HANDLE_FLAG_INHERIT, HANDLE_FLAG_INHERIT ) ;
#   }
#}

=head1 FUNCTIONS

=over

=cut

=item win32_fake_pipe

    $fake_pipe = win32_fake_pipe ;

Just a safe wrapper around listen(). Chooses a port number >= 2048
to listen on. Call $fake_pipe->parent_handle() after spawning
the children to do the accept() and get a valid "pipe" handle.

We need to build an IPC::Run::Pipe class and make a 
IPC::Run::Pipe::Win32tcp subclass of it, I think.

=cut

## _pump is only called in other processes, not in the parent process.  If
## it were any bigger, I'd compile it on-the-fly or move it to a separate
## package.  As it it, I don't think it takes up much unused space in the 
## parent.  Can move it to a separate module later to make loading it faster
## in the children (it needs almost no other modules).
sub _pump {
   require IPC::Run ;
   my ( $parent_pid, $parent_start_time, $debug, $child_label, @opts ) = @ARGV ;

   binmode STDIN  if grep /binmodein/,  @opts ;
   binmode STDOUT if grep /binmodeout/, @opts ;

   $| = 1 ;
   select STDERR ; $| = 1 ; select STDOUT ;

   $child_label ||= "pump" ;
   IPC::Run::_debug_init(
      $parent_pid,
      $parent_start_time,
      $debug,
      fileno STDERR,
      $child_label,
   ) ;
   _debug "Entered" if _debugging_details ;
   # No need to close all fds; win32 doesn't seem to pass any on to us.
   $| = 1 ;
   my $buf ;
   my $total_count = 0 ;
   while (1) {
      my $count = sysread STDIN, $buf, 10_000 ;
      last unless $count ;
      $total_count += $count ;
      if ( _debugging_gory_details ) {
	 my $msg = "'$buf'" ;
	 substr( $msg, 100, -1 ) = '...' if length $msg > 100 ;
	 $msg =~ s/\n/\\n/g ;
	 $msg =~ s/\r/\\r/g ;
	 $msg =~ s/\t/\\t/g ;
	 $msg =~ s/([\000-\037\177-\277])/sprintf "\0x%02x", ord $1/eg ;
	 _debug sprintf( "%5d chars: ", $count ), $msg ;
      }
      print $buf ;
   }

   _debug "Exiting, transferred $total_count chars" if _debugging_details ;

   ## Perform a graceful socket shutdown.  Windows defaults to SO_DONTLINGER,
   ## which should cause a "graceful shutdown in the background" on sockets.
   ## but that's only true if the process closes the socket manually, it
   ## seems; if the process exits and lets the OS clean up, the OS is not
   ## so kind.  STDOUT is not always a socket, of course, but it won't hurt
   ## to close a pipe and may even help.  With a closed source OS, who
   ## can tell?
   ##
   ## In any case, this close() is one of the main reasons we have helper
   ## processes; if the OS closed socket fds gracefully when an app exits,
   ## we'd just redirect the client directly to what is now the pump end 
   ## of the socket.  As it is, however, we need to let the client play with
   ## pipes, which don't have the abort-on-app-exit behavior, and then
   ## adapt to the sockets in the helper processes to allow the parent to
   ## select.
   ##
   ## Possible alternatives / improvements:
   ## 
   ## 1) use helper threads instead of processes.  I don't trust perl's threads
   ## as of 5.005 or 5.6 enough (which may be myopic of me).
   ##
   ## 2) figure out if/how to get at WaitForMultipleObjects() with pipe
   ## handles.  May be able to take the Win32 handle and pass it to 
   ## Win32::Event::wait_any, dunno.
   ## 
   ## 3) Use Inline::C or a hand-tooled XS module to do helper threads.
   ## This would be faster than #1, but would require a ppm distro.
   ##
   close STDOUT ;
   close STDERR ;
}

## When threaded Perls get good enough, we should use threads here.
## The problem with threaded perls is that they dup() all sorts of
## filehandles and fds and don't allow sufficient control over
## closing off the ones we don't want.
sub _spawn_pumper {
   my ( $stdin, $stdout, $debug_fd, $child_label, @opts ) = @_ ;
   my ( $stdin_fd, $stdout_fd ) = ( fileno $stdin, fileno $stdout ) ;

   my $process ;
   my $cmd_line = join( " ",
      qw(perl -MIPC::Run::Win32Helper -e _pump ),
      $$, $^T, IPC::Run::_debugging_level(), qq{"$child_label"},
      @opts
   ) ;

   open SAVEIN,  "<&STDIN"  or croak "$! saving STDIN" ;
   open SAVEOUT, ">&STDOUT" or croak "$! saving STDOUT" ;
   open SAVEERR, ">&STDERR" or croak "$! saving STDERR" ;
   _dont_inherit \*SAVEIN ;
   _dont_inherit \*SAVEOUT ;
   _dont_inherit \*SAVEERR ;
   open STDIN,  "<&$stdin_fd"  or croak "$! dup2()ing $stdin_fd (pumper's STDIN)" ;
   open STDOUT, ">&$stdout_fd" or croak "$! dup2()ing $stdout_fd (pumper's STDOUT)" ;
   open STDERR, ">&$debug_fd" or croak "$! dup2()ing $debug_fd (pumper's STDERR/debug_fd)" ;

   _debug "pump cmd line: ", $cmd_line ;

   Win32::Process::Create( 
      $process,
      $^X,
      $cmd_line,
      1,  ## Inherit handles
      NORMAL_PRIORITY_CLASS,
      ".",
   ) or croak "$!: Win32::Process::Create()" ;

   open STDIN,  "<&SAVEIN"  or croak "$! restoring STDIN" ;
   open STDOUT, ">&SAVEOUT" or croak "$! restoring STDOUT" ;
   open STDERR, ">&SAVEERR" or croak "$! restoring STDERR" ;
   close SAVEIN             or croak "$! closing SAVEIN" ;
   close SAVEOUT            or croak "$! closing SAVEOUT" ;
   close SAVEERR            or croak "$! closing SAVEERR" ;

   close $stdin  or croak "$! closing pumper's stdin in parent" ;
   close $stdout or croak "$! closing pumper's stdout in parent" ;
   # Don't close $debug_fd, we need it, as do other pumpers.

   # Pause a moment to allow the child to get up and running and emit
   # debug messages.  This does not always work.
   #   select undef, undef, undef, 1 if _debugging_details ;

   _debug "_spawn_pumper pid = ", $process->GetProcessID ;
}


my $next_port = 2048 ;
my $loopback  = inet_aton "127.0.0.1" ;
my $tcp_proto = getprotobyname('tcp');
croak "$!: getprotobyname('tcp')" unless defined $tcp_proto ;

sub _socket {
   my ( $server ) = @_ ;
   $server ||= gensym ;
   my $client = gensym ;

   my $listener = gensym ;
   socket $listener, PF_INET, SOCK_STREAM, $tcp_proto
      or croak "$!: socket()";
   setsockopt $listener, SOL_SOCKET, SO_REUSEADDR, pack("l", 0)
      or croak "$!: setsockopt()";

   my $port ;
PORT_FINDER_LOOP:
   {
      $port = $next_port ;
      $next_port = 0 if ++$next_port > 65_535 ;
      unless ( bind $listener, sockaddr_in( $port, INADDR_ANY ) ) {
        croak "$!: $! on port $port" unless $! =~ /TODO!!!!/ ;
        goto PORT_FINDER_LOOP 
      }
   }

   _debug "win32 port = $port" if _debugging_details ;

   listen $listener, my $queue_size = 1
      or croak "$!: listen()" ;

   {
      socket $client, PF_INET, SOCK_STREAM, $tcp_proto
         or croak "$!: socket()";

      my $paddr = sockaddr_in($port, $loopback );

      connect $client, $paddr
         or croak "$!: connect()" ;
    
      croak "$!: accept" unless defined $paddr ;

      ## The windows "default" is SO_DONTLINGER, which should make
      ## sure all socket data goes through.  I have my doubts based
      ## on experimentation, but nothing prompts me to set SO_LINGER
      ## at this time...
      setsockopt $client, IPPROTO_TCP, TCP_NODELAY, pack("l", 0)
	 or croak "$!: setsockopt()";
   }

   {
      _debug "accept()ing on port $port" ;
      my $paddr = accept( $server, $listener ) ;
      croak "$!: accept()" unless defined $paddr ;
   }

   _debug
      "win32 _socket = ( ", fileno $server, ", ", fileno $client, " ) on port $port" ;
   return ( $server, $client ) ;
}


sub win32_fake_pipe {
   my ( $dir, $debug_fd, $parent_handle, $binmode ) = @_ ;

   confess "Undefined \$dir" unless defined $dir ;

   my $self = bless {
      ## Two ends of the socket that connect from the pump thread to the main
      ## thread.
      PARENT_HANDLE        => undef,
      PUMP_SOCKET_HANDLE   => undef,

      ## Two ends of the pipe that connect from the pump thread to
      ## the child process.
      CHILD_HANDLE         => gensym,
      PUMP_PIPE_HANDLE     => gensym,
   }, "IPC::Run::Win32FakePipe" ;

   @{$self}{qw( PARENT_HANDLE PUMP_SOCKET_HANDLE )} = _socket $parent_handle ;

_debug "PUMP_SOCKET_HANDLE = ", fileno $self->{PUMP_SOCKET_HANDLE} ;
#my $buf ;
#$buf = "write on child end of " . fileno( $self->{WRITE_HANDLE} ) . "\n\n\n\n\n" ;
#POSIX::write(fileno $self->{WRITE_HANDLE}, $buf, length $buf) or warn "$! in syswrite" ;
#$buf = "write on parent end of " . fileno( $self->{CHILD_HANDLE} ) . "\r\n" ;
#POSIX::write(fileno $self->{CHILD_HANDLE},$buf, length $buf) or warn "$! in syswrite" ;
#   $self->{CHILD_HANDLE}->autoflush( 1 ) ;
#   $self->{WRITE_HANDLE}->autoflush( 1 ) ;

   ## Now fork off a data pump and arrange to return the correct fds.
   if ( $dir eq "<" ) {
      pipe $self->{CHILD_HANDLE}, $self->{PUMP_PIPE_HANDLE}
         or croak "$! opening child pipe" ;
_debug "CHILD_HANDLE = ", fileno $self->{CHILD_HANDLE} ;
_debug "PUMP_PIPE_HANDLE = ", fileno $self->{PUMP_PIPE_HANDLE} ;
   }
   else {
      pipe $self->{PUMP_PIPE_HANDLE}, $self->{CHILD_HANDLE}
         or croak "$! opening child pipe" ;
_debug "CHILD_HANDLE = ", fileno $self->{CHILD_HANDLE} ;
_debug "PUMP_PIPE_HANDLE = ", fileno $self->{PUMP_PIPE_HANDLE} ;
   }

   ## No child should ever see this.
   _dont_inherit $self->{PARENT_HANDLE} ;

   ## We clear the inherit flag so these file descriptors are not inherited.
   ## It'll be dup()ed on to STDIN/STDOUT/STDERR before CreateProcess is
   ## called and *that* fd will be inheritable.
   _dont_inherit $self->{PUMP_SOCKET_HANDLE} ;
   _dont_inherit $self->{PUMP_PIPE_HANDLE} ;
   _dont_inherit $self->{CHILD_HANDLE} ;

   ## Need to return $self so the HANDLEs don't get freed.
   ## Return $self, $parent_fd, $child_fd
   my ( $parent_fd, $child_fd ) = (
      fileno $self->{PARENT_HANDLE},
      fileno $self->{CHILD_HANDLE}
   ) ;

   ## Both PUMP_..._HANDLEs will be closed, no need to worry about
   ## inheritance.
   _spawn_pumper(
      ( $dir eq "<" )
	 ? ( $self->{PUMP_SOCKET_HANDLE}, $self->{PUMP_PIPE_HANDLE} )
	 : ( $self->{PUMP_PIPE_HANDLE}, $self->{PUMP_SOCKET_HANDLE} ),
      $debug_fd,
      "pump$dir$parent_fd",
      ## parent side should always be binmoded
      $binmode               ? ( "binmodein", "binmodeout" )
         : ( $dir eq "<" )   ? ( "binmodein"               )
                             : (              "binmodeout" ),
   ) ;

{
my $foo ;
confess "PARENT_HANDLE no longer open"
   unless POSIX::read( $parent_fd, $foo, 0 ) ;
}

   _debug "win32_fake_pipe = ( $parent_fd, $child_fd )" ;
   return ( $self, $parent_fd, $child_fd ) ;
}


=item win32_parse_cmd_line

   @words = win32_parse_cmd_line( q{foo bar 'baz baz' "bat bat"} ) ;

returns 4 words. This parses like the bourne shell (see
the bit about shellwords() in L<Text::ParseWords>), assuming we're
trying to be a little cross-platform here.  The only difference is
that "\" is *not* treated as an escape except when it precedes 
punctuation, since it's used all over the place in DOS path specs.

TODO: globbing? probably not (it's unDOSish).

TODO: shebang emulation? Probably, but perhaps that should be part
of Run.pm so all spawned processes get the benefit.

LIMITATIONS: shellwords dies silently on malformed input like 

   a\"

=cut

sub win32_parse_cmd_line {
   my $line = shift ;
   $line =~ s{(\\[^[:punct:]])}{\\$1}g ;
   return shellwords $line ;
}


=item win32_spawn

Spawns a child process, possibly with STDIN, STDOUT, and STDERR (file descriptors 0, 1, and 2, respectively) redirected.

B<LIMITATIONS>.

Cannot redirect higher file descriptors due to lack of support for this in the
Win32 environment.

This can be worked around by marking a handle as inheritable in the
parent (or leaving it marked; this is the default in perl), obtaining it's
Win32 handle with C<Win32API::GetOSFHandle(FH)> or
C<Win32API::FdGetOsFHandle($fd)> and passing it to the child using the command
line, the environment, or any other IPC mechanism (it's a plain old integer).
The child can then use C<OsFHandleOpen()> or C<OsFHandleOpenFd()> and possibly
C<<open FOO ">&BAR">> or C<<open FOO ">&$fd>> as need be.  Ach, the pain!

Remember to check the Win32 handle against INVALID_HANDLE_VALUE.

=cut

sub _save {
   my ( $saved, $saved_as, $fd ) = @_ ;

   ## We can only save aside the original fds once.
   return if exists $saved->{$fd} ;

   my $saved_fd = IPC::Run::_dup( $fd ) ;
   _dont_inherit $saved_fd ;

   $saved->{$fd} = $saved_fd ;
   $saved_as->{$saved_fd} = $fd ;

   _dont_inherit $saved->{$fd} ;
}

sub _dup2_gently {
   my ( $saved, $saved_as, $fd1, $fd2 ) = @_ ;
   _save $saved, $saved_as, $fd2 ;

   if ( exists $saved_as->{$fd2} ) {
      ## The target fd is colliding with a saved-as fd, gotta bump
      ## the saved-as fd to another fd.
      my $orig_fd = delete $saved_as->{$fd2} ;
      my $saved_fd = IPC::Run::_dup( $fd2 ) ;
      _dont_inherit $saved_fd ;

      $saved->{$orig_fd} = $saved_fd ;
      $saved_as->{$saved_fd} = $orig_fd ;
   }
   _debug "moving $fd1 to kid's $fd2" if _debugging_details ;
   IPC::Run::_dup2_rudely( $fd1, $fd2 ) ;
}

sub win32_spawn {
   my ( $cmd, $ops) = @_ ;

   ## NOTE: The debug pipe write handle is passed to pump processes as STDOUT.
   ## and is not to the "real" child process, since they would not know
   ## what to do with it...unlike Unix, we have no code executing in the
   ## child before the "real" child is exec()ed.
   
   my %saved ;      ## Map of parent's orig fd -> saved fd
   my %saved_as ;   ## Map of parent's saved fd -> orig fd, used to
                    ## detect collisions between a KFD and the fd a
		    ## parent's fd happened to be saved to.
   
   for my $op ( @$ops ) {
      _dont_inherit $op->{FD}  if defined $op->{FD} ;

      if ( defined $op->{KFD} && $op->{KFD} > 2 ) {
	 ## TODO: Detect this in harness()
	 ## TODO: enable temporary redirections if ever necessary, not
	 ## sure why they would be...
	 ## 4>&1 1>/dev/null 1>&4 4>&-
         croak "Can't redirect fd #", $op->{KFD}, " on Win32" ;
      }

      ## This is very similar logic to IPC::Run::_do_kid_and_exit().
      if ( defined $op->{TFD} ) {
	 unless ( $op->{TFD} == $op->{KFD} ) {
	    _dup2_gently \%saved, \%saved_as, $op->{TFD}, $op->{KFD} ;
	    _dont_inherit $op->{TFD} ;
	 }
      }
      elsif ( $op->{TYPE} eq "dup" ) {
         _dup2_gently \%saved, \%saved_as, $op->{KFD1}, $op->{KFD2}
            unless $op->{KFD1} == $op->{KFD2} ;
      }
      elsif ( $op->{TYPE} eq "close" ) {
	 _save \%saved, \%saved_as, $op->{KFD} ;
	 IPC::Run::_close( $op->{KFD} ) ;
      }
      elsif ( $op->{TYPE} eq "init" ) {
	 ## TODO: detect this in harness()
         croak "init subs not allowed on Win32" ;
      }
   }

   my $process ;
   my $cmd_line = join " ", map {
      if ( /["\s]/ ) {
	 s/"/\\"/g ;
	 qq{"$_"} ;
      }
      else {
         $_ ;
      }
   } @$cmd ;

   _debug "cmd line: ", $cmd_line ;

   Win32::Process::Create( 
      $process,
      $cmd->[0],
      $cmd_line,
      1,  ## Inherit handles
      NORMAL_PRIORITY_CLASS,
      ".",
   ) or croak "$!: Win32::Process::Create()" ;

   for my $orig_fd ( keys %saved ) {
      IPC::Run::_dup2_rudely( $saved{$orig_fd}, $orig_fd ) ;
      IPC::Run::_close( $saved{$orig_fd} ) ;
   }

   return ( $process->GetProcessID(), $process ) ;
}


=back

=head1 AUTHOR

Barries Slaymaker <barries@slaysys.com>.  Funded by Perforce Software, Inc.

=head1 COPYRIGHT

Copyright 2001, Barrie Slaymaker, All Rights Reserved.

You may use this under the terms of either the GPL 2.0 ir the Artistic License.

=cut

1 ;
