# This -*- perl -*- script writes the Makefile for libwww-perl
# Time-stamp: "1998-08-13 21:16:35 MDT"
require 5.004;
use strict;
use ExtUtils::MakeMaker;

WriteMakefile(
   NAME          => 'MIDI-Perl',
   VERSION_FROM  => 'lib/MIDI.pm',
   'dist'        => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
);

package MY;

sub libscan
{ # Determine things that should *not* be installed
    my($self, $path) = @_;
    return '' if $path =~ m/~/;
    $path;
}
