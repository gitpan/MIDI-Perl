# This -*- perl -*- script writes the Makefile for MIDI-Perl
# Time-stamp: "2002-11-16 02:12:43 MST"
require 5.004;
use strict;
use ExtUtils::MakeMaker;

WriteMakefile(
   NAME          => 'MIDI-Perl',
   VERSION_FROM  => 'lib/MIDI.pm',
   ABSTRACT  => 'read, compose, modify, and write MIDI files',
   'dist'        => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
);

package MY;

sub libscan
{ # Determine things that should *not* be installed
    my($self, $path) = @_;
    return '' if $path =~ m/~/;
    $path;
}

__END__
