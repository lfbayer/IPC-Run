use ExtUtils::MakeMaker;

eval { require IO::Pty ; } or warn <<TOHERE ;
WARNING: IO::Pty not found.  '<pty<' and '>pty>' will not be supported
         until IO::Pty is installed.

TOHERE

WriteMakefile(
    NAME          => 'IPC::Run',
#    PMLIBDIRS    => [ 'Run' ],
    VERSION_FROM  => 'Run.pm', # finds $VERSION
);
