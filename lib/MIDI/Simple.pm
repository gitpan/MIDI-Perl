# Time-stamp: "1998-08-24 10:50:30 MDT"
package MIDI::Simple;
use MIDI;
use Carp;
#use strict; # Feh.
##use vars qw(@ISA @EXPORT $VERSION
##            &make_opus($\@) &write_score($$\@)
##            &read_score($) &dump_score(\@)
##           );
require Exporter;
@ISA = qw(Exporter);
@EXPORT = qw(new_score n r make_opus write_score read_score dump_score);
$VERSION = 0.61;
$Debug = 1;

=head1 NAME

MIDI::Simple - simple functional/procedural interface to MIDI functionality

=head1 SYNOPSIS

 use MIDI::Simple;  # uses MIDI, which uses everything it needs.
 new_score;
 push @Score,
  ['text_event', 0, 'www.ely.anglican.org/parishes/camgsm/chimes.html'],
  ['text_event', 0, 'Lord through this hour/ be Thou our guide'],
  ['text_event', 0, 'so, by Thy power/ no foot shall slide'],
  ['patch_change', 0, 1, 8]; # Patch 8 = Celesta
 n c1, f, qn, Cs2; n F; n Ds; n hn, Gs_d1;
 n qn, Cs; n Ds; n F; n hn, Cs;
 n qn, F; n Cs; n Ds; n hn, Gs_d1;
 n qn, Gs_d1; n Ds; n F; n hn, Cs;
 write_score 'chimes3y.mid', $Tempo, @Score;

=head1 DESCRIPTION

I<This module is in beta -- let me know if you run into any problems.>

This module provides two related but distinct bits of functionality:
1) a mini-language (implemented as procedures) for composing by adding
notes to a score structure; and 2) simple functions for reading and
writing scores, specifically the scores you make with the composition
language.

This module sits on top of all the MIDI modules -- notably MIDI::Score
(so you should read L<MIDI::Score>) -- and is meant to serve as a
basic interface to them.

=head1 CONCEPTS

The below section is a description of MIDI::Simple's concise
composition language.  It's not meant to be the end-all and be-all of
composition.  In fact, in many cases you might be better off using the
abc language (which I link to specs for, from the MIDI-Perl homepage).

Consider these notes:

 @Score = (
  ['note', 0, 96, 1, 25, 96],
  ['note', 96, 96, 1, 29, 96],
  ['note', 192, 96, 1, 27, 96],
  ['note', 288, 192, 1, 20, 96],
  ['note', 480, 96, 1, 25, 96],
  ['note', 576, 96, 1, 27, 96]
 );

You could express these notes as above; but composing in such terms is
tedious and overly verbose.  The basic problem is that there's a high
degree of redundancy in this set of notes -- they're all at the same
volume (96), they're all on channel 1, their note-numbers are all
between 20 and 29, the notes all start where the previous note left
off, and all but one of them is 96 ticks long.  Moreover, the order
they're listed in, is cronological order; and this happens to be the
order one thinks of busic as being in, more or less.

To capture these facts, I have implemented two procedures (and I call
them this, instead of "functions", because they have side-effects --
i.e., they affect things other than the variables they're called on)
which express notes and rests, and whose options capture the
redundancy I've discussed above.  (The chief reason I made these
procedures, and not stateful OOP methods, is for sake of concision and
simplicity.)

The procure "n" (for "note") adds a note whose parameters you specify,
to the score in @Score, at a start-time given in $Time, and then
increments $Time by the duration of the note.  (The procedure "r" (for
"rest") does increment $Time, but doesn't add anything to @Score,
since there's no necessary MIDI representation for "and now be quiet
for this long".)
(These variables all exist in the current package -- but if you don't
know what that means, don't worry.)

As a preliminary specification of the syntax, let's say that you
specify these parameters (which are comma-separated, and can appear
in any order) each as a letter followed by a number, as such:
"d[NUMBER]" (e.g., "d96") declares the note as having that duration in ticks;
"c[NUMBER]" declares the note as being on that channel;
"v[VOLUME]" declares the note as having that volume;
"n[NUMBER]" declares the note as being on that MIDI note number.
And let's say that these parameters can be in any order.

So consider this translation of the above score:

 $Time = 0;
 n  d96, c1, v96, n25;    # was:  ['note', 0, 96, 1, 25, 96],
 n  d96, c1, v96, n29;    # was:  ['note', 96, 96, 1, 29, 96],
 n  d96, c1, v96, n27;    # was:  ['note', 192, 96, 1, 27, 96],
 n d192, c1, v96, n20;    # was:  ['note', 288, 192, 1, 20, 96],
 n  d96, c1, v96, n25;    # was:  ['note', 480, 96, 1, 25, 96],
 n  d96, c1, v96, n27;    # was:  ['note', 576, 96, 1, 27, 96],
 # and now these events are in @Score

These are context-free, unambiguous expressions of the notes we had
in the above score.  However, we haven't tackled the redundancy problem.
So let's say that every call to C<n> or C<r> inherits the options from
the previous call to C<n> or C<r>.  In other words, each note-or-rest
is just like the previous note-or-rest, except for whatever (if any)
parameters you specify.  So, this:

 n  d96, c1, v96, n27;
 n d192, c1, v96, n20;

can be more concisely expressed as:

 n  d96, c1, v96, n27;
 n d192,          n20;

or, rewrapped:

 n d96, c1, v96, n27;  n d192, n20;

And this:

 n d192 c1 n20 v96;
 n  d96 c1 n20 v96;

is the same as:

 n d192 c1 n20 v96;
 n  d96;

Now, the options for each call to C<n> or C<r> are stored in the
global variables (in the current package) $Duration, $Channel, $Note,
and $Volume; and the parameters you give for C<n> or C<r> just end up
getting parsed and stored in these variables for use in future calls.
You can change them yourself, in fact; so, this:

 n d192 c1 n20 v96;
 n  d96;
 r;
 n;

does basically the same thing as:

 $Duration = 192; $Channel = 1; $Note = 20; $Volume = 96;
 push @Score, ['note', $Time, $Duration, $Channel, $Note, $Volume ];
 $Time += $Duration;

 $Duration =  96;
 push @Score, ['note', $Time, $Duration, $Channel, $Note, $Volume ];
 $Time += $Duration;

 $Time += $Duration;

 push @Score, ['note', $Time, $Duration, $Channel, $Note, $Volume ];
 $Time += $Duration;

Now, that's all there is to the basic idea of how C<n> and <r> work.
However, people like to think in mezzo-fortes and quarter notes and
octaves and B-flats, not volume numbers and ticks and MIDI note
numbers, so I've added extensions for these.

=head2 Volume Parameters

These entirely ad-hoc abbreviatons

   abbreviation
  /         means the same as   /(what these abbreviations stand for)
 |         /                   /
 ppp =>   v1,  # pianississimo
 pp  =>  v12,  # pianissimo
 p   =>  v24,  # piano
 mp  =>  v48,  # mezzopiano
 m   =>  v64,  # mezzo / medio / meta` / middle / whatever
 mf  =>  v80,  # mezzoforte
 f   =>  v96,  # forte
 ff  => v112,  # fortissimo
 fff => v127,  # fortississimo

These values, incidentally, are stored in (in fact, come from)
%MIDI::Simple::Volume -- which you can redefine as you like.
See the source for the internal format.

=head2 Duration Parameters

We could have an absolute list of values such that "half note equals
192 ticks" and so on, but I've decided to keep it flexible, and have
abbreviations for note durations be relative to a global variable,
$Tempo, which says how many ticks a quarter note should last for.

So "qn" (quarter note) is 1 times the value of $Tempo, "hn" (half
note) is 2 * $Tempo, "en" (eighth note) is .5 * $Tempo, and so on.

The complete list of abbreviations (and the multipliers to $Tempo they
stand for) currently supported, is:

 'qn' =>  1,    'dqn' => 1.5,  'tqn' => (2/3),
 'en' => .5,    'den' => .75,  'ten' => (1/3),
 'sn' => .25,   'dsn' => .375, 'tsn' => (1/6),
 'wn' =>  4,    'dwn' => 6,
 'hn' =>  2,    'dhn' => 3,    'thn' => (4/3),

("d" means dotted, and "t" means triplet)

These values, incidentally, are stored in (in fact, come from)
%MIDI::Simple::Length -- which you can redefine (presumably only to
add to) as you like.  See the source for the internal format.

I strongly recommend that you use "96" as your opus's tick parameter
(AKA the second parameter to C<write_score>) I<and> as the value for
$Tempo (which it defaults to anyway).  Then when you want to change
actual performance tempo, stick "set_tempo"s thruout your score (and
presumably have one at the beginning too).  Remember that the syntax
for "set_tempo" is:

 push @Score,  ['set_tempo', $Time, $microseconds_per_qn];

and that's MICROseconds -- so 600000 means just .6 seconds.

The reason 96 is so magic, by the way, is that it's 3 * (2 ** 5), so
that all kinds of bizarre fractions of 96 (including and beyond the
ones in %MIDI::Simple::Length) yield whole numbers of ticks -- which
is crucial, since a MIDI file can't store fractional tick-counts.  If
you use a $Tempo or tick count of just any number, you risk having
round-off errors, leading to, say, an eighth-note triplet adding up to
a few ticks stort of a the time of a quarter note.

=head2 Note Parameters

You can specify the note's pitch in two absolute formats:
"n[NUMBER]" (where NUMBER is the MIDI note number), or as
"[NOTE][OCTAVE]", where OCTAVE is a number 0 to 10, and NOTE is
one of:

 C,
 Cs, Df, Csharp, or Dflat,   (all synonymous)
 D,
 Ds, Ef, Dsharp, or Eflat,   (all synonymous)
 E,
 F,
 Fs, Gf, Fsharp, or Gflat,   (all synonymous)
 G,
 Gs, Af, Gsharp, or Aflat,   (all synonymous)
 A,
 As, Bf, Asharp, or Bflat,   (all synonymous)
 B

(These values, incidentally, are stored in (in fact, come from)
%MIDI::Simple::Note -- which you can redefine (presumably only to add
to) as you like.  See the source for the internal format.)

All of these specifications set the variable $Octave, which represents
the number of the current octave.  $Octave is used for the
interpretation of these other ways to represent notes, which I call
relative specifications:

 [NOTE]                    e.g., A,   or Bflat,   or Bf

This means that note (e.g., 'A') in octave number $Octave.

 [NOTE]_d[OCTAVECOUNT]      e.g., A_d3, or Bflat_d3, or Bf_d3

This means that note ('A') that many octaves below $Octave.

 [NOTE]_u[OCTAVECOUNT]      e.g., A_u3, or Bflat_u3, or Bf_u3

This means that note ('A') that many octaves above $Octave.

Note that neither [NOTE]d[OCTAVECOUNT] nor [NOTE]u[OCTAVECOUNT]
affect the variable $Octave -- only "n[NUMBER]" and "[NOTE][OCTAVE]"
affect $Octave.

So, to summarize, recall this score structure we've been using as
a whole and in bits:

 @Score = (
  ['text_event', 0, 'www.ely.anglican.org/parishes/camgsm/chimes.html'],
  ['text_event', 0, 'Lord through this hour/ be Thou our guide'],
  ['text_event', 0, 'so, by Thy power/ no foot shall slide'],
  ['patch_change', 0, 1, 8],
  ['note', 0, 96, 1, 25, 96],
  ['note', 96, 96, 1, 29, 96],
  ['note', 192, 96, 1, 27, 96],
  ['note', 288, 192, 1, 20, 96],
  ['note', 480, 96, 1, 25, 96],
  ['note', 576, 96, 1, 27, 96],
  ['note', 672, 96, 1, 29, 96],
  ['note', 768, 192, 1, 25, 96],
  ['note', 960, 96, 1, 29, 96],
  ['note', 1056, 96, 1, 25, 96],
  ['note', 1152, 96, 1, 27, 96],
  ['note', 1248, 192, 1, 20, 96],
  ['note', 1440, 96, 1, 20, 96],
  ['note', 1536, 96, 1, 27, 96],
  ['note', 1632, 96, 1, 29, 96],
  ['note', 1728, 192, 1, 25, 96]
 );

The concise way to express this with C<n> and C<r> would be:

 $Time = (); $Tempo = 96; @Score = (
  ['text_event', 0, 'www.ely.anglican.org/parishes/camgsm/chimes.html'],
  ['text_event', 0, 'Lord through this hour/ be Thou our guide'],
  ['text_event', 0, 'so, by Thy power/ no foot shall slide'],
  ['patch_change', 0, 1, 8],
 );
 n qn, Cs2; n F; n Ds; n hn, Gs_d1;
 n qn, Cs; n Ds; n F; n hn, Cs;
 n qn, F; n Cs; n Ds; n hn, Gs_d1;
 n qn, Gs_d1; n Ds; n F; n hn, Cs;

=head2 List of n/r's State Variables

  varname     default value from new_score
 ---------   ---------------------------------------
 @Score       ( ['text_event', 0, "$0 at " . scalar(localtime) ] )
 $Time        0
 $Duration    96
 $Channel     0
 $Octave      5
 $Tempo       96
 $Note        60
 $Volume      64

=head2 READ THE SOURCE!

The implementations of C<n> and C<r> are simple enough that I I<strongly>
encourage you to read their source.  Their source, in fact, is I<much>
shorter than the docs on them that you've just read.

=cut

%Volume = ( # I've simply made up these values from more or less nowhere.
# You no like?  Change 'em at runtime, or just use "v64" or whatever,
# to  specify the volume as a number 1-127.
 'ppp' =>   1,  # pianississimo
 'pp'  =>  12,  # pianissimo
 'p'   =>  24,  # piano
 'mp'  =>  48,  # mezzopiano
 'm'   =>  64,  # mezzo / medio / meta` / middle / whatever
 'mf'  =>  80,  # mezzoforte
 'f'   =>  96,  # forte
 'ff'  => 112,  # fortissimo
 'fff' => 127,  # fortississimo
);

%Length = ( # this list should be rather uncontroversial.
 # The numbers here are multiples of a quarter note's length
 # The abbreviations are qn for "quarter note", dqn for "dotted quarter
 #  note", tqn for "triplet quarter note"
 'qn' =>  1,    'dqn' => 1.5,  'tqn' => (2/3),
 'en' => .5,    'den' => .75,  'ten' => (1/3),
 'sn' => .25,   'dsn' => .375, 'tsn' => (1/6),
 'wn' =>  4,    'dwn' => 6,    # is there even such a thing as a dwn or a thn?
 'hn' =>  2,    'dhn' => 3,    'thn' => (4/3),
 # yes, these fractions could lead to round-off errors, I suppose
 # but note that 96 * all of these == a WHOLE NUMBER!!!!!
);

%Note = (
 'C'      =>  0,
 'Cs'     =>  1, 'Df'     =>  1, 'Csharp' =>  1, 'Dflat'  =>  1,
 'D'      =>  2,
 'Ds'     =>  3, 'Ef'     =>  3, 'Dsharp' =>  3, 'Eflat'  =>  3,
 'E'      =>  4,
 'F'      =>  5,
 'Fs'     =>  6, 'Gf'     =>  6, 'Fsharp' =>  6, 'Gflat'  =>  6,
 'G'      =>  7,
 'Gs'     =>  8, 'Af'     =>  8, 'Gsharp' =>  8, 'Aflat'  =>  8,
 'A'      =>  9,
 'As'     => 10, 'Bf'     => 10, 'Asharp' => 10, 'Bflat'  => 10,
 'B'      => 11,
);
###########################################################################
# MIDI::Simple language:

=head1 PROCEDURES

=over

=item new_score

This resets the above-listed state variables (in the current package)
to their given values.

=cut

sub new_score {
  my $package = (caller)[0];
  @{"$package\::Score"} = (['text_event', 0, "$0 at " . scalar(localtime) ]);
  ${"$package\::Time"} = 0;
  ${"$package\::Duration"} = 96; # a good average tempo
  ${"$package\::Channel"} = 0;
  ${"$package\::Octave"} = 5;
  ${"$package\::Tempo"} = 96; # ticks per qn
  ${"$package\::Note"} = 60; # middle C. why not.
  ${"$package\::Volume"} = 64; # normal
}
###########################################################################

=item n [parameters]

Push to @Score a note whose parameters come from the state variables,
overridden by (and possibly affected by) the given parameters.

=cut

sub n { # a note
  my $package = (caller)[0];
  &MIDI::Simple::_parse_options($package, @_);
  my $note = 
    ['note',
     ${"$package\::Time"},
     ${"$package\::Duration"},
     ${"$package\::Channel"},
     ${"$package\::Note"},
     ${"$package\::Volume"},
    ];
  push @{"$package\::Score"}, $note;
  ${"$package\::Time"} += ${"$package\::Duration"};
#  print map("<$_> ", @$note ), "\n" if $Debug;
  return;
}
###########################################################################

=item r [parameters]

Just like n, except that no note in actually pushed to @Score -- but
all other state variables can be affected, notably $Time.

=cut

sub r { # a rest
  my $package = (caller)[0];
  &MIDI::Simple::_parse_options($package, @_);
  ${"$package\::Time"} += ${"$package\::Duration"};
  return;
}
###########################################################################

sub _parse_options { # common parser for n and r options
  my( $package, @args ) = @_;
  croak "no package for _parse_options" unless $package;
  foreach my $arg (@args) {
    if($arg      =~ m<^d(\d+)$>s) {
      ${"$package\::Duration"} = $1;
    } elsif($arg =~ m<^v(\d+)$>s) {
      croak "Volume out of range: $1" if $1 > 127;
      ${"$package\::Volume"} = $1;
    } elsif($arg =~ m<^c(\d+)$>s) {
      croak "Channel out of range: $1" if $1 > 15;
      ${"$package\::Channel"} = $1;
    } elsif($arg =~ m<^n(\d+)$>s) {
      croak "Octave out of range: $1" if $1 > 127;
      ${"$package\::Note"} = $1;
      ${"$package\::Octave"} = int($1 / 12);

    # the complex ones follow:
    } elsif( exists( $MIDI::Simple::Volume{$arg} )) { # Volume specifications
      ${"$package\::Volume"} = $MIDI::Simple::Volume{$arg};

    } elsif( exists( $MIDI::Simple::Length{$arg} )) { # Length specifications
      ${"$package\::Duration"} =
         ${"$package\::Tempo"} * $MIDI::Simple::Length{$arg};

    } elsif( $arg =~ m<^([A-Za-z]+)((?:_[du])?\d+)?$>s   # Note specifications
	   #  and exists( $MIDI::Simple::Note{$1}
           )
    {
      my $note = $MIDI::Simple::Note{$1};
      my $octave = ${"$package\::Octave"};
      my $o_spec = $2;

#      print "note<$1> => <$note> ; octave<$2>\n" if $Debug;

      if (! length($o_spec)){
        # noop
      } elsif ($o_spec =~ m<^(\d+)$>s) {
        ${"$package\::Octave"} = $octave = $1;
	croak "Octave out of range: \"$1\" in \"$arg\"" if $1 > 10;
      } elsif ($o_spec =~ m<^_d(\d+)$>s) {
	$octave -= $1;
	$octave = 0 if $octave < 0;
      } elsif ($o_spec =~ m<^_u(\d+)$>s) {
	$octave += $1;
	$octave = 10 if $octave > 10;
      } else {
        die "Unexpected error 5176123";
      }
      ${"$package\::Note"} = $note + $octave * 12;
      # 12 = number of MIDI notes in an octive

    } else {
      croak "Unknown note/rest option: \"$arg\"" if length($arg);
    }
  }
  return;
}

=over

=cut

# End of MIDI::Simple language stuff
###########################################################################

=head1 FUNCTIONS

These are basic functions for doing things with your score, whether in
@Score, or in some other array.

=over

=item $opus = write_score I<$filespec>, I<$ticks>, I<@Score>

Writes @Score to the filespec in $output (e.g,
"../../samples/funk2.midi"), with $ticks as its tick parameters (AKA
"divisions").  This function actually calls the function C<make_opus>,
below, and if you capture the output of write_score, you'll get the
opus created, if you want it for anything.

=cut

sub write_score ($$\@) {
  my($out, $ticks, $score_r) = @_[0,1,2];

  croak "First parameter to MIDI::Simple::write_score can't be null\n"
    unless( ref($out) || length($out) );
  croak "Ticks can't be 0" unless $ticks;
  croak "Third parameter to MIDI::Simple::write_score must be an array\n"
    unless ref($score_r);

  carp "Writing a score with no notes!" unless @$score_r;
  my $opus = &MIDI::Simple::make_opus($ticks, $score_r);
	# (overriding the prototype)

# $opus->dump( { 'dump_tracks' => 1 } );

  if(ref($out)) {
    $opus->write_to_handle($out);
  } else {
    $opus->write_to_file($out);
  }
  return $opus; # capture it if you want it.
}

###########################################################################

=item ($ticks, @Score) = read_score I<$filespec>

Reads the file whose filespec is given in $filespec, interprets it as
a score, and returns that file's tick parameter, followed by the
@Score, which you should capture as above.

Note that this will die if the file has more than one eventful track.

If ever you have to make a score out of a single track in a multitrack
file, read the file into an $opus, then get the track you want into
$track, and then just do:

 $ticks = $opus->ticks;
 @Score =  @{
             ( MIDI::Score::events_r_to_score_r($track->events_r) )[0]
            };

=cut

sub read_score ($) {
  my $in = $_[0];
  my($track, @eventful_tracks);
  croak "First parameter to MIDI::Simple::read_score can't be null\n"
    unless( ref($in) || length($in) );

  my $in_switch = ref($in) ? 'from_handle' : 'from_file';
  my $opus = MIDI::Opus->new({ $in_switch => $in });

  @eventful_tracks = grep( scalar(@{$_->events_r}),  $opus->tracks );
  if(@eventful_tracks == 0) {
    croak "Opus from $in has NO eventful tracks to consider as a score!\n";
  } elsif (@eventful_tracks > 1) {
    croak
      "Opus from $in has too many (" .
	scalar(@eventful_tracks) . ") tracks to be a score.\n";
  } # else OK...
  $track = $eventful_tracks[0];
#print scalar($track->events), " events in track\n";

  # If ever you want just a single track as a score, here's how:
  my $score_r =  ( MIDI::Score::events_r_to_score_r($track->events_r) )[0];
#print scalar(@$score_r), " notes in score\n";
  return($opus->ticks, @$score_r);
}
###########################################################################

=item dump_score @Score;

Dumps @Score's contents, via C<print> (so you can C<select> an
output handle for it).  Currently this is in this kind of uninspiring
format:

  ['note', 0, 96, 1, 25, 96],
  ['note', 96, 96, 1, 29, 96],

as it is (currently) just a call to &MIDI::Score::dump_score; but in
the future I may (should?) make it output in C<n>/C<r> notation.

=cut

sub dump_score (\@) {
  return &MIDI::Score::dump_score(@_);
}
###########################################################################

=item $opus = make_opus I<$ticks>, I<@Score>

Makes an opus (a MIDI::Opus object) out of @Score, setting the
opus's tick parameter (AKA "divisions") to $ticks.  The opus is,
incidentally, format 0, with one track.

=cut

sub make_opus ($\@) {
  # Make a format-0 one-track MIDI out of this score.
  my($ticks, $score_r) = @_[0,1];
  carp "Encoding a score with no notes!" unless @$score_r;
  my $events_r = ( MIDI::Score::score_r_to_events_r($score_r) )[0];
  carp "Creating a track with no events!" unless @$events_r;

  my $opus =
    MIDI::Opus->new({ 'ticks'  => $ticks,
		      'format' => 0,
		      'tracks' => [ MIDI::Track->new({
						    'events' => $events_r
						   }) ]
		    });
  return $opus;
}
###########################################################################

=back

=head1 THINK ABOUT IT

What happens if you have routines where you localize @Score and then
call C<n> and C<r>?

=head1 AUTHOR

Sean M. Burke C<sburke@netadventure.net>

=cut

1;

__END__
