###########################################################################
# Time-stamp: "1998-08-13 23:34:46 MDT"
package MIDI;
use MIDI::Opus;
use MIDI::Track;
use MIDI::Event;

$Debug = 0; # currently doesn't do anything
$VERSION = 0.51;

# MIDI.pm doesn't do much other than 1) 'use' all the necessary submodules
# 2) provide some publicly useful hashes, 3) house a few private routines
# common to the MIDI::* modules, and 4) contain POD, glorious POD.

=head1 NAME

MIDI -- read, compose, modify, and write MIDI files.

=head1 SYNOPSIS

 use MIDI;
 $opus = MIDI::Opus->new;
 ...etc...

=head1 DESCRIPTION

This suite of modules provides routines for reading, composing, modifying,
and writing MIDI files.

From FOLDOC (C<http://wombat.doc.ic.ac.uk/foldoc/>):

=over

B<MIDI, Musical Instrument Digital Interface>
                                       
E<lt>multimedia, file formatE<gt> (MIDI /mi'-dee/, /mee'-dee/) A
hardware specification and protocol used to communicate note and effect
information between synthesisers, computers, music keyboards,
controllers and other electronic music devices. [...]

The basic unit of information is a "note on/off" event which includes
a note number (pitch) and key velocity (loudness). There are many
other message types for events such as pitch bend, patch changes and
synthesizer-specific events for loading new patches etc.

There is a file format for expressing MIDI data which is like a dump
of data sent over a MIDI port. [...]

=back

=head1 COMPONENTS

The MIDI suite consists of these modules:

L<MIDI> (which you're looking at), L<MIDI::Opus>, L<MIDI::Track>, and
L<MIDI::Event>.  All of these contain documentation in pod format.
You should read all these pods.

For your reference, there is also a document in pod format which is not
itself an actual module: L<MIDI::Filespec>.  It is an old version
of the MIDI file specification.

=head1 INTRODUCTION

This suite of modules is basically object-oriented.  MIDI opuses
("songs") are represented as objects belonging to the class
MIDI::Opus.  An opus contains tracks, which are objects belonging to
the class MIDI::Track.  A track will generally contain a list of
events, where each event is a list consisting of a command, a
delta-time, and some number of parameters.  In other words, opuses and
tracks are objects, and the events in a track comprise a LoL (and if you
don't know what an LoL is, you must read L<perllol>).

While this suite does provide some functionality accessible only if
you're comfortable with various kinds of references, and while there
are some options that deal with the guts of MIDI encoding, you can (I
hope) get along just fine with just a basic grasp of the MIDI
"standard", and a command of LoLs.  I have tried, at various points in
this documentation, to point out what things are not likely to be of
use to the casual user.

=head1 TO DO

Hopefully provide I<much> better documentation in a later
release of this module.

Maybe have a MIDI cookbook of commonly used short scripts?

Have a more abstract level of abstraction than MIDI events, such that
1) notes are represented as a single event, instead of each a pair of
a note_on event and a note_off event; and 2) events are represented as
having a given time-offset from start-of-track, instead of this fishy
delta-time business, which is not exactly prime for composition.
Actually, I'm currently working on this -- expect it to show up as
another attribute of a track object, maybe $track->score, beside
$track->events, and stored similarly to the events LoL.

Have modules carp/croak instead of warn/die.

Have more modules "use strict".

B<A PLEA>: Currently this suite can only read/write MIDI data from/to
MIDI I<files>.  However, it would be desirable to have realtime access
to a MIDI device -- at least on systems where a MIDI device (whether
thru a hardware port or as a virtual sequencer in a sound card) is
accessable as a virtual file (C</dev/midi0>, C</dev/midi>,
C</dev/sequencer>, or whatever).  However, I have no such MIDI devices
(much less ports) at hand for development and testing.  But if I<you>
have such devices (I'm thinking a Linuxer with a synth hooked to their
MIDI port), and if you want to help me experiment with directly
accessing them from Perl, then please email me.  I already have a
pretty good idea of how it should work -- but as always, the proof is
as much in the pudding as the devil is in the details.

=head1 GOODIES

The bare module MIDI.pm doesn't I<do> much more than C<use> the
component submodules.  But it does provide some hashes you might find
useful:

=over

=cut

###########################################################################
# Note numbers => a representation of them

=item C<%MIDI::note2number> and C<%MIDI::number2note>

C<%MIDI::note2number> correponds MIDI note numbers to a more
comprehensible representation (e.g., 68 to 'G#4');
C<%MIDI::number2note> is the reverse.  Have a look at the source
to see the contents of the hash.

=cut
@note2number{0 .. 127} = (
# (Do)        (Re)         (Mi)  (Fa)         (So)         (La)        (Ti)
 'C0', 'C#0', 'D0', 'D#0', 'E0', 'F0', 'F#0', 'G0', 'G#0', 'A0', 'A#', 'B0',
 'C1', 'C#1', 'D1', 'D#1', 'E1', 'F1', 'F#1', 'G1', 'G#1', 'A1', 'A#', 'B1',
 'C2', 'C#2', 'D2', 'D#2', 'E2', 'F2', 'F#2', 'G2', 'G#2', 'A2', 'A#', 'B2',
 'C3', 'C#3', 'D3', 'D#3', 'E3', 'F3', 'F#3', 'G3', 'G#3', 'A3', 'A#', 'B3',
 'C4', 'C#4', 'D4', 'D#4', 'E4', 'F4', 'F#4', 'G4', 'G#4', 'A4', 'A#', 'B4',
 'C5', 'C#5', 'D5', 'D#5', 'E5', 'F5', 'F#5', 'G5', 'G#5', 'A5', 'A#', 'B5',
 'C6', 'C#6', 'D6', 'D#6', 'E6', 'F6', 'F#6', 'G6', 'G#6', 'A6', 'A#', 'B6',
 'C7', 'C#7', 'D7', 'D#7', 'E7', 'F7', 'F#7', 'G7', 'G#7', 'A7', 'A#', 'B7',
 'C8', 'C#8', 'D8', 'D#8', 'E8', 'F8', 'F#8', 'G8', 'G#8', 'A8', 'A#', 'B8',
 'C9', 'C#9', 'D9', 'D#9', 'E9', 'F9', 'F#9', 'G9', 'G#9', 'A9', 'A#', 'B9',
 'C10','C#10','D10','D#10','E10','F10','F#10','G10',
  # Note number 69 reportedly == A440, under a default tuning.
  # and note 60 = Middle C
);
%number2note = reverse %note2number;
# Note how I deftly avoid having to figure out how to represent a flat mark
#  in ASCII.

###########################################################################
#  ****     TABLE 1  -  General MIDI Instrument Patch Map      ****
# (groups sounds into sixteen families, w/8 instruments in each family)
#  Note that I call the map 0-127, not 1-128.

=item C<%MIDI::patch2number> and C<%MIDI::number2patch>

C<%MIDI::patch2number> correponds General MIDI patch numbers
(0 to 127) to English names (e.g., 79 to 'Ocarina');
C<%MIDI::number2patch> is the reverse.  Have a look at the source
to see the contents of the hash.

=cut
@patch2number{0 .. 127} = (   # The General MIDI map: patches 0 to 127
#0: Piano
 "Acoustic Grand", "Bright Acoustic", "Electric Grand", "Honky-Tonk",
 "Electric Piano 1", "Electric Piano 2", "Harpsichord", "Clav",
# Chrom Percussion
 "Celesta", "Glockenspiel", "Music Box", "Vibraphone",
 "Marimba", "Xylophone", "Tubular Bells", "Dulcimer",

#16: Organ
 "Drawbar Organ", "Percussive Organ", "Rock Organ", "Church Organ",
 "Reed Organ", "Accoridan", "Harmonica", "Tango Accordian",
# Guitar
 "Acoustic Guitar(nylon)", "Acoustic Guitar(steel)",
 "Electric Guitar(jazz)", "Electric Guitar(clean)",
 "Electric Guitar(muted)", "Overdriven Guitar",
 "Distortion Guitar", "Guitar Harmonics",

#32: Bass
 "Acoustic Bass", "Electric Bass(finger)",
 "Electric Bass(pick)", "Fretless Bass",
 "Slap Bass 1", "Slap Bass 2", "Synth Bass 1", "Synth Bass 2",
# Strings
 "Violin", "Viola", "Cello", "Contrabass",
 "Tremolo Strings", "Orchestral Strings", "Orchestral Strings", "Timpani",

#48: Ensemble
 "String Ensemble 1", "String Ensemble 2", "SynthStrings 1", "SynthStrings 2",
 "Choir Aahs", "Voice Oohs", "Synth Voice", "Orchestra Hit",
# Brass
 "Trumpet", "Trombone", "Tuba", "Muted Trumpet",
 "French Horn", "Brass Section", "SynthBrass 1", "SynthBrass 2",

#64: Reed
 "Soprano Sax", "Alto Sax", "Tenor Sax", "Baritone Sax",
 "Oboe", "English Horn", "Bassoon", "Clarinet",
# Pipe
 "Piccolo", "Flute", "Recorder", "Pan Flute",
 "Blown Bottle", "Skakuhachi", "Whistle", "Ocarina",

#80: Synth Lead
 "Lead 1 (square)", "Lead 2 (sawtooth)", "Lead 3 (calliope)", "Lead 4 (chiff)",
 "Lead 5 (charang)", "Lead 6 (voice)", "Lead 7 (fifths)", "Lead 8 (bass+lead)",
# Synth Pad
 "Pad 1 (new age)", "Pad 2 (warm)", "Pad 3 (polysynth)", "Pad 4 (choir)",
 "Pad 5 (bowed)", "Pad 6 (metallic)", "Pad 7 (halo)", "Pad 8 (sweep)",

#96: Synth Effects
 "FX 1 (rain)", "FX 2 (soundtrack)", "FX 3 (crystal)", "FX 4 (atmosphere)",
 "FX 5 (brightness)", "FX 6 (goblins)", "FX 7 (echoes)", "FX 8 (sci-fi)",
# Ethnic
 "Sitar", "Banjo", "Shamisen", "Koto",
 "Kalimba", "Bagpipe", "Fiddle", "Shanai",

#112: Percussive
 "Tinkle Bell", "Agogo", "Steel Drums", "Woodblock",
 "Taiko Drum", "Melodic Tom", "Synth Drum", "Reverse Cymbal",
# Sound Effects
 "Guitar Fret Noise", "Breath Noise", "Seashore", "Bird Tweet",
 "Telephone Ring", "Helicopter", "Applause", "Gunshot",
);
%number2patch = reverse %patch2number;

###########################################################################
#     ****    TABLE 2  -  General MIDI Percussion Key Map    ****
# (assigns drum sounds to note numbers. MIDI Channel 10 is for percussion)

=item C<%MIDI::notenum2percussion> and C<%MIDI::percussion2notenum>

C<%MIDI::notenum2percussion> correponds General MIDI Percussion Keys
to English names (e.g., 56 to 'Cowbell') -- but note that only numbers
35 to 81 (inclusive) are defined; C<%MIDI::percussion2notenum> is the
reverse.  Have a look at the source to see the contents of the hash.

=cut

@notenum2percussion{35 .. 81} = (
 'Acoustic Bass Drum', 'Bass Drum 1', 'Side Stick', 'Acoustic Snare',
 'Hand Clap',

 # the forties 
 'Electric Snare', 'Low Floor Tom', 'Closed Hi-Hat', 'High Floor Tom',
 'Pedal Hi-Hat', 'Low Tom', 'Open Hi-Hat', 'Low-Mid Tom', 'Hi-Mid Tom',
 'Crash Cymbal 1',

 # the fifties
 'High Tom', 'Ride Cymbal 1', 'Chinese Cymbal', 'Ride Bell', 'Tambourine',
 'Splash Cymbal', 'Cowbell', 'Crash Cymbal 2', 'Vibraslap', 'Ride Cymbal 2',

 # the sixties
 'Hi Bongo', 'Low Bongo', 'Mute Hi Conga', 'Open Hi Conga', 'Low Conga',
 'High Timbale', 'Low Timbale', 'High Agogo', 'Low Agogo', 'Cabasa',

 # the seventies
 'Maracas', 'Short Whistle', 'Long Whistle', 'Short Guiro', 'Long Guiro',
 'Claves', 'Hi Wood Block', 'Low Wood Block', 'Mute Cuica', 'Open Cuica',

 # the eighties
 'Mute Triangle', 'Open Triangle',
);
%percussion2notenum = reverse %notenum2percussion;

###########################################################################

=back

=head1 BRIEF GLOSSARY

This glossary defines just a few terms, just enough so you can
(hopefully) make some sense of the documentation for this suite of
modules.  If you're going to do anything serious with these modules,
however, you I<should really> invest in a good book about the MIDI
standard -- see the References.

B<channel>: a logical channel to which control changes and patch
changes apply, and in which MIDI (note-related) events occur.

B<control>: one of the various numeric parameters associated with a
given channel.  Like S registers in Hayes-set modems, MIDI controls
consist of a few well-known registers, and beyond that, it's
patch-specific and/or sequencer-specific.

B<delta time>: the time (in ticks) that a sequencer should wait
between playing the previous event and playing the current event.

B<meta-event>: any of a mixed bag of events whose common trait is
merely that they are similarly encoded.  Most meta-events apply to all
channels, unlike events, which mostly apply to just one channel.

B<opus>: the term I prefer for a piece of music, as represented in
MIDI.  Most specs use the term "song", but I think that this
falsely implies that MIDI files represent vocal pieces.

B<patch>: an electronic model of the sound of a given notional
instrument.

B<running status>: a form of modest compression where an event lacking
an event command byte (a "status" byte) is to be interpreted as having
the same event command as the preceding event -- which may, in turn,
lack a status byte and may have to be interpreted as having the same
event command as I<its> previous event, and so on back.

B<song>: what some MIDI specs call a song, I call an opus.

B<sequencer>: a device or program that interprets and acts on MIDI
data.  This prototypically refers to synthesizers or drum machines,
but can also refer to more limited devices, such as mixers or even
lighting control systems.

B<status>: a synonym for "event".

B<sysex>: a chunk of binary data encapsulated in the MIDI data stream,
for whatever purpose.

B<text event>: any of the several meta-events (one of which is
actually called 'text_event') that conveys text.  Most often used to
just label tracks, note the instruments used for a track, or to
provide metainformation about copyright, performer, and piece title
and author.

B<tick>: the timing unit in a MIDI opus.

B<variable-length encoding>: an encoding method identical to what Perl
calls the 'w' (BER, Basic Encoding Rules) pack/unpack format for
integers.

=head1 REFERENCES

Christian Braut.  I<The Musician's Guide to Midi.>  ISBN 0782112854.

I'll keep a list of other references and good stuff at
the URL C<http://alf8.speech.cs.cmu.edu/~sburke/pub/perl_midi/>

=head1 AUTHOR

Sean M. Burke C<sburke@netadventure.net>

=cut

###########################################################################
sub _dump_quote {
  # Used variously by some MIDI::* modules.  Might as well keep it here.
  return
    join(", ",
	map
	 { # the cleaner-upper function
	   if(!length($_)) { # empty string
	     "''";
	   } elsif( m/^-?\d+(?:\.\d+)?$/s ) { # a number
	     $_;
	   } elsif( # text with junk in it
	      s<([^\x20\x21\x23\x27-\x3F\x41-\x5B\x5D-\x7E])>
	       <'\\x'.(unpack("H2",$1))>eg
	     ) {
	     "\"$_\"";
	   } else { # text with no junk in it
	     s<'><\\'>g;
	     "\'$_\'";
	   }
	 }
	 (@_)
	);
}
###########################################################################

1;

__END__
