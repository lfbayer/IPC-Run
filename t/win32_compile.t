#!/usr/bin/perl -w

=head1 NAME

win32_compile.t - See if IPC::Run::Win32Helper compiles, even on Unix

=cut

use strict ;

use Test ;

BEGIN {
   $INC{$_} = 1 for qw( Win32/Process.pm Win32API/File.pm ) ;

   package Win32API::File ;

   use vars qw( @ISA @EXPORT ) ;

   @ISA = qw( Exporter ) ;
   @EXPORT = qw(
      GetOsFHandle 
      OsFHandleOpen
      OsFHandleOpenFd
      FdGetOsFHandle
      SetHandleInformation
      SetFilePointer

      HANDLE_FLAG_INHERIT
      INVALID_HANDLE_VALUE

      createFile
      WriteFile
      ReadFile
      CloseHandle

      FILE_ATTRIBUTE_TEMPORARY
      FILE_FLAG_DELETE_ON_CLOSE
      FILE_FLAG_WRITE_THROUGH

      FILE_BEGIN
   ) ;

   eval "sub $_ { 1 }" for @EXPORT ;

   use Exporter ;

   package Win32::Process ;

   use vars qw( @ISA @EXPORT ) ;

   @ISA = qw( Exporter ) ;
   @EXPORT = qw(
      NORMAL_PRIORITY_CLASS
   ) ;

   eval "sub $_ {}" for @EXPORT ;

   use Exporter ;
}

sub Socket::IPPROTO_TCP() { undef }

package main ;

use IPC::Run::Win32Helper ;
use IPC::Run::Win32IO ;

plan tests => 1 ;

ok 1 ;

