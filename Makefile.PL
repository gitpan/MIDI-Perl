# This -*- perl -*- script writes the Makefile for MIDI-Perl
# Time-stamp: "1998-10-27 19:59:10 MST"
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

__END__
