use ExtUtils::MakeMaker;

print <<'TOHERE' ;

   If you experience problems while running make test, please run
   the failing scripts using a command like:

      make test TEST_FILES=t/foo.t TEST_VERBOSE=1 IPCRUNDEBUG=4 > foo.out 2>&1

   (use nmake on Windows) and sending foo.out with your problem report.
   Bonus air miles awarded for writing a small, simple exploit script :).

   You may also use the IPCRUNDEBUG=1 (or 2, 3, or 4) trick with your own
   programs, see perldoc IPC::Run for details.

   See perldoc IPC::Run for details on the experimental nature of
   pty and Win32 support.

   <barbie tm="Mattel">Subprocesses are *HARD*.</barbie>

TOHERE

sub pty_warn {
   warn "WARNING: $_[0].\nWARNING: '<pty<', '>pty>' $_[1] not work.\n\n";
   last ;
}

my @conditional_prereqs;

if ( $^O !~ /Win32/ ) {
   for ( eval { require IO::Pty ; IO::Pty->VERSION } ) {
      s/_//g if defined ;
      pty_warn "IO::Pty not found", "will" unless defined ;
      push @conditional_prereqs, IO::Pty => 1.00;
   }
}

WriteMakefile(
    NAME          => 'IPC::Run',
    VERSION_FROM  => 'Run.pm',
    PREREQ_PM => {
        @conditional_prereqs,
    }
);


sub MY::libscan {
   package MY ;
   my $self = shift ;
   my ( $path ) = @_ ;
   return '' if /\.sw[a-z]$/ ;
   return '' unless length $self->SUPER::libscan( $path ) ;
   return $path ;
}
