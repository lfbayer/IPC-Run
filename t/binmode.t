#!/usr/bin/perl -w

=head1 NAME

binary.t - Test suite for IPC::Run binary functionality

=cut

## Handy to have when our output is intermingled with debugging output sent
## to the debugging fd.
$| = 1 ;
select STDERR ; $| = 1 ; select STDOUT ;

use strict ;

use Test ;

use IPC::Run qw( harness run binary ) ;

sub Win32_MODE() ;
*Win32_MODE = \&IPC::Run::Win32_MODE ;

my $bin_text = "Hello World\r\n" ;

my $text     = $bin_text ;
$text =~ s/\r//g if Win32_MODE ;

my $nl_text  = $bin_text ;
$nl_text =~ s/\r//g ;

my @perl    = ( $^X ) ;

my $emitter_script = q{ binmode STDOUT ; print "Hello World\r\n" } ;
my @emitter = ( @perl, '-e', $emitter_script ) ;

my $echoer_script = q{ binmode STDIN ; binmode STDOUT ; print <> } ;
my @echoer = ( @perl, '-e', $echoer_script ) ;

my $in ;
my $out ;
my $err ;

sub f($) {
   my $s = shift ;
   $s =~ s/([\000-\027])/sprintf "\\0x%02x", ord $1/ge ;
   $s
}

my @tests = (
## Parsing tests
sub { ok eval { harness [], '>', binary, \$out } ? 1 : $@, 1 } ,
sub { ok eval { harness [], '>', binary, "foo" } ? 1 : $@, 1 },
sub { ok eval { harness [], '<', binary, \$in  } ? 1 : $@, 1 },
sub { ok eval { harness [], '<', binary, "foo" } ? 1 : $@, 1 },

## Testing from-kid now so we can use it to test stdin later
sub { ok run \@emitter, ">", \$out },
sub { ok f $out, f $text, "no binary" },

sub { ok run \@emitter, ">", binary, \$out },
sub { ok f $out, f $bin_text, "out binary" },

sub { ok run \@emitter, ">", binary( 0 ), \$out },
sub { ok f $out, f $text, "out binary 0" },

sub { ok run \@emitter, ">", binary( 1 ), \$out },
sub { ok f $out, f $bin_text, "out binary 1" },

## Test to-kid
sub { ok run \@echoer, "<", \$text, ">", \$out },
sub { ok f $out, f $text, "echoer, no binary" },

sub { ok run \@echoer, "<", \$text, ">", binary, \$out },
sub { ok f $out, f $bin_text, "echoer, out binary" },

sub { ok run \@echoer, "<", binary, \$text, ">", binary, \$out },
sub { ok f $out, f $text, "echoer, in, out binary" },

sub { ok run \@echoer, "<", binary, \$bin_text, ">", binary, \$out },
sub { ok f $out, f $bin_text, "echoer, in, out binary, sending \r" },

sub { ok run \@echoer, "<", binary( 0 ), \$text, ">", binary, \$out },
sub { ok f $out, f $bin_text, "echoer, in binary 0, out binary, sending \r" },

sub { ok run \@echoer, "<", binary( 1 ), \$bin_text, ">", binary, \$out },
sub { ok f $out, f $bin_text, "in binary 1, out binary, sending \r" },

) ;

plan tests => scalar @tests ;

$_->() for ( @tests ) ;
