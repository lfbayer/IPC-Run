package IPC::Run::Pty ;

=head1 NAME

IPC::Run::Pty -- Class implementing psuedo-terminals for IPC::Run

=head1 SYNOPSIS

=head1 DESCRIPTION

This class allows creation of pty harnesses for IPC::Run.  This includes
support for multiple children sharing a pty.

Dont use or require this unless you need it or you can be sure that
IO::Pty is present.

Does not inherit from IO::Pty, because we need to separate instantiation
from opening (and perhaps re-opening) IO::Pty.

=head1 CREDITS

Uses IO::Pty, informed by Expect.pm.

=cut

use strict ;
use vars qw( @ISA ) ;

use Carp ;
use IO::Pty ;

use fields qw(
   PTY
) ;

sub new {
   my $class = shift ;
   $class = ref $class || $class ;
   my $self = {} ;
   bless $self, $class ;

}

sub init_parent() {
   croak "Don't spawn IO::Pty's directly, please" ;
   my IPC::Run::Pty $self = shift ;
   $self->{PTY}->autoflush() ;
}


sub init_child() {
   my IPC::Run::Pty $self= shift ;

   my $pty = $self->{PTY}->slave() ;
   close( $self->{PTY} ) ;

   close STDIN ;
   close STDOUT ;
   close STDERR ;

   open( STDIN,  "<&", $pty->fileno ) || croak "Failed to open pty for stdin";
   open( STDOUT, ">&", $pty->fileno ) || croak "Failed to open pty for stdout";
   open( STDERR, ">&", $pty->fileno ) || croak "Failed to open pty for stderr";
}
