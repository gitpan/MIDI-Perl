# Time-stamp: "1998-10-18 23:23:59 MDT"
package MIDI::Simple;
use MIDI;
use Carp;
use strict 'vars';
use vars qw(@ISA @EXPORT $VERSION $Debug
            %package
            %Volume @Note %Note %Length
            &make_opus($\@) &write_score($$\@)
            &read_score($) &dump_score(\@)
           );
require Exporter;
@ISA = qw(Exporter);
$VERSION = 0.700;
$Debug = 0;

@EXPORT = qw(
 new_score n r noop interval note_map
 Score   Time   Duration   Channel   Octave   Tempo   Notes   Volume
 Score_r Time_r Duration_r Channel_r Octave_r Tempo_r Notes_r Volume_r
 Cookies Cookies_r Self
 write_score read_score dump_score make_opus synch
 is_note_spec is_relative_note_spec is_absolute_note_spec
 number_to_absolute number_to_relative

 key_after_touch control_change patch_change channel_after_touch
 pitch_wheel_change set_sequence_number text_event copyright_text_event
 track_name instrument_name lyric marker cue_point

 text_event_08 text_event_09 text_event_0a text_event_0b text_event_0c
 text_event_0d text_event_0e text_event_0f

 end_track set_tempo smpte_offset time_signature key_signature
 sequencer_specific raw_meta_event

 sysex_f0 sysex_f7
 song_position song_select tune_request raw_data
);     # _test_proc

local %package = ();
# hash of package-scores: accessible as $MIDI::Simple::package{"packagename"}
# but REALLY think twice about writing to it, OK?
# To get at the current package's package-score object, just call
#  $my_object = Self;

=head1 NAME

MIDI::Simple - procedural/OOP interface for MIDI composition

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
 write_score 'chimes3y.mid';

=head1 DESCRIPTION

I<This module is in beta> -- a sloooowly maturing meta.  Let me know if
you run into any problems, and feel free to suggest features.

This module is somewhat incompatible with the MIDI::Simple versions
before .700.

I think I've settled on (i.e., basically frozen) the basic interface
for this module, and will now hopefully only add functionality.

This module sits on top of all the MIDI modules -- notably MIDI::Score
(so you should skim L<MIDI::Score>) -- and is meant to serve as a
basic interface to them.  By composition, I mean composing anew; you
can use this module to add to or modify existing MIDI files, but that
functionality is to be considered expermental.

This module provides two related but distinct bits of functionality:
1) a mini-language (implemented as procedures that can double as
methods) for composing by adding notes to a score structure;
and 2) simple functions for reading and writing scores, specifically
the scores you make with the composition language.

The fact that this module's interface is both procedural and
object-oriented makes it a definite two-headed beast.  The guts of
the source code is not for the faint of heart.

=cut

%Volume = ( # I've simply made up these values from more or less nowhere.
# You no like?  Change 'em at runtime, or just use "v64" or whatever,
# to specify the volume as a number 1-127.
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
 'C'  =>  0,
 'Cs' =>  1, 'Df' =>  1, 'Csharp' =>  1, 'Dflat' =>  1,
 'D'  =>  2,
 'Ds' =>  3, 'Ef' =>  3, 'Dsharp' =>  3, 'Eflat' =>  3,
 'E'  =>  4,
 'F'  =>  5,
 'Fs' =>  6, 'Gf' =>  6, 'Fsharp' =>  6, 'Gflat' =>  6,
 'G'  =>  7,
 'Gs' =>  8, 'Af' =>  8, 'Gsharp' =>  8, 'Aflat' =>  8,
 'A'  =>  9,
 'As' => 10, 'Bf' => 10, 'Asharp' => 10, 'Bflat' => 10,
 'B'  => 11,
);

@Note = qw(C Df  D Ef  E   F Gf  G Af  A Bf  B);
# These are for converting note numbers to names, via, e.g., $Note[2]
# These must be a subset of the keys to %Note.
# You may choose to have these be your /favorite/ names for the particular
# notes.  I've taken a stab at that myself.
###########################################################################

=head2 OBJECT STRUCTURE

A MIDI::Simple object is a data structure with the following
attributes:

=over

=item Score

This is a list of all the notes (each a listref) that constitute this
one-track musical piece.  Scores are explained in L<MIDI::Score>.
You probably don't need to access the Score attribute directly, but be
aware that this is where all the notes you make with C<n> events go.

=item Time

This is a non-negative integer expressing the time, in ticks from the
start-time of the MIDI piece, that the next note pushed to the Score
will have.

=item Channel

This is a number in the range [0-15] that specifies the current default
channel for note events.

=item Duration

This is a non-negative (presumably nonzero) number expressing, in
ticks, the current default length of note events, or rests.

=item Octave

This is a number in the range [0-10], expressing what the current
default octave number is.  This is used for figuring out exactly
what note-pitch is meant by a relative note-pitch specification
like "A".

=item Notes

This is a list (presumably non-empty) of note-pitch specifications,
I<as note numbers> in the range [0-127].

=item Volume

This is an integer in the range [0-127] expressing the current default
volume for note events.

=item Tempo

This is an integer expressing the number of ticks a quarter note
occupies.  It's currently 96, and you shouldn't alter it unless you
I<really> know what you're doing.  If you want to control the tempo of
a piece, use the C<set_tempo> routine, instead.

=item Cookies

This is a hash that can be used by user-defined object-methods for
storing whatever they want.

=back

Each package that you call the procedure C<new_score> from, has a
default MIDI::Simple object associated with it, and all the above
attributes are accessible as:

  @Score $Time $Channel $Duration $Octave
  @Notes $Volume $Tempo %Cookies

(Although I doubt you'll use these from any package other than
"main".)  If you don't know what a package is, don't worry about it.
Just consider these attributes synonymous with the above-listed
variables.  Just start your programs with

  use MIDI::Simple;
  new_score;

and you'll be fine.

=head2 ROUTINE/METHOD/PROCEDURE

MIDI::Simple provides some pure functions (i.e., things that take
input, and return output, and that's all they do), but what you're
mostly interested in its routines.  By "routine" I mean a bit of code
that you call, whether as a procedure or as a method.

Here I'm using "procedure" to mean a routine you call like this:

  name(parameters...);
  # or, just maybe:
  &name(parameters);

(In technical terms, I mean a non-method subroutine that can have
side effects.)  And I'm using "method" to mean a routine you call like
this:

  $object->name(parameters)

So bear these terms in mind when you see routines below that act
one one, or the other, or both.

=head2 MAIN ROUTINES

These are the most important routines:

=over

=item new_score()  or  $obj = MIDI::Simple->new_score()

As a procedure, this intializes the package's default object (Score,
etc.).  As a method, this is a constructor, returning a new
MIDI::Simple object.  Neither form takes any parameters.

=cut

=item n(...parameters...)  or  $obj->n(...parameters...)

This uses the parameters given (and/or the state variables like
Volume, Channel, Notes, etc) to add a new note to the Score.  Then it
moves Time ahead as appropriate.  These parameters may affect the
other state variables.  See the section "Parameters For n/r/noop",
below.

=cut

sub n { # a note
  my($am_method, $it) = (ref($_[0]) eq "MIDI::Simple")
    ? (1, shift @_)
    : (0, ($package{ (caller)[0] } ||= &_package_object( (caller)[0] )) );
  &MIDI::Simple::_parse_options($it, @_);
  foreach my $note_val (@{$it->{"Notes"}}) {
    # which should presumably not be a null list
    unless($note_val =~ /^\d+$/) {
      carp "note value \"$note_val\" from Notes is non-numeric!  Skipping.";
      next;
    }
    push @{$it->{"Score"}},
      ['note',
       int(${$it->{"Time"}}),
       int(${$it->{"Duration"}}),
       int(${$it->{"Channel"}}),
       int($note_val),
       int(${$it->{"Volume"}}),
      ];
  }
  ${$it->{"Time"}} += ${$it->{"Duration"}};
  return;
}
###########################################################################

=item r(...parameters...)  or  $obj->r(...parameters...)

This is exactly like C<n>, except it never pushes anything to Score,
but moves ahead Time.  (In other words, there is no such thing as a
rest-event; it's just a item during which there are no note-events
playing.)

=cut

sub r { # a rest
  my($am_method, $it) = (ref($_[0]) eq "MIDI::Simple")
    ? (1, shift @_)
    : (0, ($package{ (caller)[0] } ||= &_package_object( (caller)[0] )) );
  &MIDI::Simple::_parse_options($it, @_);
  ${$it->{"Time"}} += ${$it->{"Duration"}};
  return;
}
###########################################################################

=item noop(...parameters...)  or  $obj->noop(...parameters...)

This is exactly like C<n> and C<r>, except it never pushes anything to
Score, I<and> never changes Time.  It is meant to be used for setting
the other state variables, i.e.: Channel, Duration, Octave, Volume,
Notes.

=cut

sub noop { # no operation
  my($am_method, $it) = (ref($_[0]) eq "MIDI::Simple")
    ? (1, shift @_)
    : (0, ($package{ (caller)[0] } ||= &_package_object( (caller)[0] )) );
  &MIDI::Simple::_parse_options($it, @_);
  return;
}

#--------------------------------------------------------------------------

sub _parse_options { # common parser for n/r/noop options
  # This is the guts of the whole module.  Understand this and you'll
  #  understand everything.
  my( $it, @args ) = @_;
  my @new_notes = ();
  print "options for _parse_options: ", map("<$_>", @args), "\n" if $Debug > 3;
  croak "no target for _parse_options" unless ref $it;
  foreach my $arg (@args) {
    next unless length($arg);
    if($arg      =~ m<^d(\d+)$>s) { # duration in ticks
      ${$it->{"Duration"}} = $1;
    } elsif($arg =~ m<^v(\d+)$>s) { # numeric volume
      croak "Volume out of range: $1" if $1 > 127;
      ${$it->{"Volume"}} = $1;
    } elsif($arg eq 'rest') { # clears the note list
      @{$it->{"Notes"}} = ();
    } elsif($arg =~ m<^c(\d+)$>s) { # channel spec
      croak "Channel out of range: $1" if $1 > 15;
      ${$it->{"Channel"}} = $1;
    } elsif($arg =~ m<^o(\d+)$>s) { # octave spec
      croak "Octave out of range: \"$1\" in \"$arg\"" if $1 > 10;
      ${$it->{"Octave"}} = int($1);

    } elsif($arg =~ m<^n?(\d+)$>s) { # explicit numeric note spec
      # note that the "n" is optional
      croak "Note out of range: $1" if $1 > 127;
      push @new_notes, $1;
      ${$it->{"Octave"}} = int($1 / 12);

    # the complex ones follow:
    } elsif( exists( $MIDI::Simple::Volume{$arg} )) { # Volume specifications
      ${$it->{"Volume"}} = $MIDI::Simple::Volume{$arg};

    } elsif( exists( $MIDI::Simple::Length{$arg} )) { # Length specifications
      ${$it->{"Duration"}} =
         ${$it->{"Tempo"}} * $MIDI::Simple::Length{$arg};

    } elsif( $arg =~ m<^([A-Za-z]+)((?:_[du])?\d+)?$>s # Note specifications
             and exists( $MIDI::Simple::Note{$1})
           )
    {
      my $note = $MIDI::Simple::Note{$1};
      my $octave = ${$it->{"Octave"}};
      my $o_spec = $2;
      print "note<$1> => <$note> ; octave_spec<$2> Octave<$octave>\n"
        if $Debug;

      if (! length($o_spec)){
        # noop
      } elsif ($o_spec =~ m<^(\d+)$>s) {
        ${$it->{"Octave"}} = $octave = $1;
        croak "Octave out of range: \"$1\" in \"$arg\"" if $1 > 10;
      } elsif ($o_spec =~ m<^_d(\d+)$>s) {
        $octave -= $1;
        $octave = 0 if $octave < 0;
      } elsif ($o_spec =~ m<^o?_u(\d+)$>s) {
        # _d3 or o_d3 to move octave up or down
        $octave += $1;
        $octave = 10 if $octave > 10;
      } else {
        die "Unexpected error 5176123";
      }
      push @new_notes, $note + $octave * 12;
      # 12 = number of MIDI notes in an octive
    } else {
      croak "Unknown note/rest option: \"$arg\"" if length($arg);
    }
  }
  @{$it->{"Notes"}} = @new_notes if @new_notes; # otherwise inherit last list
  return;
}

# Internal-use proc: create a package object for the package named.
sub _package_object {
  my $package = $_[0] || die "no package!!!";
  no strict;
  print "Linking to package $package\n" if $Debug;
  $package{$package} = bless {
    # note that these are all refs, not values
    "Score" => \@{"$package\::Score"},
    "Time" => \${"$package\::Time"},
    "Duration" => \${"$package\::Duration"},
    "Channel" => \${"$package\::Channel"},
    "Octave" => \${"$package\::Octave"},
    "Tempo" => \${"$package\::Tempo"},
    "Notes" => \@{"$package\::Notes"},
    "Volume" => \${"$package\::Volume"},
    "Cookies" => \%{"$package\::Cookies"},
  };

  &_init_score($package{$package});
  return $package{$package};
}

=back

=cut

###########################################################################

sub new_score {
  my $p1 = $_[0];
  my $it;
  if($p1 eq "MIDI::Simple") { # I'm a method!
    print "~ new_score as a MIDI::Simple constructor\n" if $Debug;
    $it = bless {};
    &_init_score($it);
  } else { # I'm a proc!
    my $cpackage = (caller)[0];
    print "~ new_score as a proc for package $cpackage\n" if $Debug;
    if( ref($package{ $cpackage }) ) {  # Already exists in %package
      print "~  reinitting pobj $cpackage\n" if $Debug;
      &_init_score(  $it = $package{ $cpackage }  );
      # no need to call _package_object
    } else {  # Doesn't exist in %package
      print "~  new pobj $cpackage\n" if $Debug;
      $package{ $cpackage } = $it = &_package_object( $cpackage );
      # no need to call _init_score
    }
  }
  return $it; # for object use, we'll be capturing this
}

sub _init_score { # Set some default initial values for the object
  my $it = $_[0];
  print "Initting score $it\n" if $Debug;
  @{$it->{"Score"}} = (['text_event', 0, "$0 at " . scalar(localtime) ]);
  ${$it->{"Time"}} = 0;
  ${$it->{"Duration"}} = 96; # a good average tempo
  ${$it->{"Channel"}} = 0;
  ${$it->{"Octave"}} = 5;
  ${$it->{"Tempo"}} = 96; # ticks per qn
  @{$it->{"Notes"}} = (60); # middle C. why not.
  ${$it->{"Volume"}} = 64; # normal
  %{$it->{"Cookies"}} = (); # empty
  return;
}

###########################################################################
###########################################################################

=head2 ATTRIBUTE METHODS

The object attributes discussed above are readable and writeable with
object methods.  For each attribute there is a read/write method, and a
read-only method that returns a reference to the attribute's value:

  Attribute ||  R/W-Method ||   RO-R-Method
  ----------++-------------++--------------------------------------
  Score     ||  Score      ||   Score_r      (returns a listref)
  Notes     ||  Notes      ||   Notes_r      (returns a listref)
  Time      ||  Time       ||   Time_r       (returns a scalar ref)
  Duration  ||  Duration   ||   Duration_r   (returns a scalar ref)
  Channel   ||  Channel    ||   Channel_r    (returns a scalar ref)
  Octave    ||  Octave     ||   Octave_r     (returns a scalar ref)
  Volume    ||  Volume     ||   Volume_r     (returns a scalar ref)
  Tempo     ||  Tempo      ||   Tempo_r      (returns a scalar ref)
  Cookies   ||  Cookies    ||   Cookies_r    (returns a hashref)

To read any of the above via a R/W-method, call with no parameters,
e.g.:

  $notes = $obj->Notes;  # same as $obj->Notes()

The above is the read-attribute ("get") form.

To set the value, call with parameters:

  $obj->Notes(13,17,22);

The above is the write-attribute ("put") form.  Incidentally, when
used in write-attribute form, the return value is the same as the
parameters, except for Score or Cookies.  (In those two cases, I've
suppressed it for efficiency's sake.)

Alternately (and much more efficiently), you can use the read-only
reference methods to read or alter the above values;

  $notes_r = $obj->Notes_r;
  # to read:
  @old_notes = @$notes_r;
  # to write:
  @$notes_r = (13,17,22);

And this is the only way to set Cookies, Notes, or Score to a (),
like so:

  $notes_r = $obj->Notes_r;
  @$notes_r = ();

Since this:

  $obj->Notes;

is just the read-format call, remember?

=cut

#--------------------------------------------------------------------------
# read-or-write methods

sub Score (;\@) { # yes, a prototype!
  my($am_method, $it) = (ref($_[0]) eq "MIDI::Simple")
    ? (1, shift @_)
    : (0, ($package{ (caller)[0] } ||= &_package_object( (caller)[0] )) );
  if(@_) {
    if($am_method){
      @{$it->{'Score'}} = @_;
    } else {
      @{$it->{'Score'}} = @{$_[0]}; # sneaky, huh!
    }
    return; # special case -- return nothing if this is a PUT
  } else {
    return @{$it->{'Score'}}; # you asked for it
  }
}

sub Cookies {
  my($it) = (ref($_[0]) eq "MIDI::Simple") ? (shift @_)
    : ($package{ (caller)[0] } ||= &_package_object( (caller)[0] ));
  %{$it->{'Cookies'}} = @_ if @_;  # Better have an even number of elements!
  return %{$it->{'Cookies'}};
}

sub Time {
  my($it) = (ref($_[0]) eq "MIDI::Simple") ? (shift @_)
    : ($package{ (caller)[0] } ||= &_package_object( (caller)[0] ));
  ${$it->{'Time'}} = $_[0] if @_;
  return ${$it->{'Time'}};
}

sub Duration {
  my($it) = (ref($_[0]) eq "MIDI::Simple") ? (shift @_)
    : ($package{ (caller)[0] } ||= &_package_object( (caller)[0] ));
  ${$it->{'Duration'}} = $_[0] if @_;
  return ${$it->{'Duration'}};
}

sub Channel {
  my($it) = (ref($_[0]) eq "MIDI::Simple") ? (shift @_)
    : ($package{ (caller)[0] } ||= &_package_object( (caller)[0] ));
  ${$it->{'Channel'}} = $_[0] if @_;
  return ${$it->{'Channel'}};
}

sub Octave {
  my($it) = (ref($_[0]) eq "MIDI::Simple") ? (shift @_)
    : ($package{ (caller)[0] } ||= &_package_object( (caller)[0] ));
  ${$it->{'Octave'}} = $_[0] if @_;
  return ${$it->{'Octave'}};
}

sub Tempo {
  my($it) = (ref($_[0]) eq "MIDI::Simple") ? (shift @_)
    : ($package{ (caller)[0] } ||= &_package_object( (caller)[0] ));
  ${$it->{'Tempo'}} = $_[0] if @_;
  return ${$it->{'Tempo'}};
}

sub Notes {
  my($it) = (ref($_[0]) eq "MIDI::Simple") ? (shift @_)
    : ($package{ (caller)[0] } ||= &_package_object( (caller)[0] ));
  @{$it->{'Notes'}} = @_ if @_;
  return @{$it->{'Notes'}};
}

sub Volume {
  my($it) = (ref($_[0]) eq "MIDI::Simple") ? (shift @_)
    : ($package{ (caller)[0] } ||= &_package_object( (caller)[0] ));
  ${$it->{'Volume'}} = $_[0] if @_;
  return ${$it->{'Volume'}};
}

#-#-#-#-#-#-#-#-##-#-#-#-#-#-#-#-#-#-#-#-##-#-#-#-#-#-#-#-##-#-#-#-#-#-#-#-
# read-only methods that return references

sub Score_r {
  my($it) = (ref($_[0]) eq "MIDI::Simple") ? (shift @_)
    : ($package{ (caller)[0] } ||= &_package_object( (caller)[0] ));
  return $it->{'Score'};
}

sub Time_r {
  my($it) = (ref($_[0]) eq "MIDI::Simple") ? (shift @_)
    : ($package{ (caller)[0] } ||= &_package_object( (caller)[0] ));
  return $it->{'Time'};
}

sub Duration_r {
  my($it) = (ref($_[0]) eq "MIDI::Simple") ? (shift @_)
    : ($package{ (caller)[0] } ||= &_package_object( (caller)[0] ));
  return $it->{'Duration'};
}

sub Channel_r {
  my($it) = (ref($_[0]) eq "MIDI::Simple") ? (shift @_)
    : ($package{ (caller)[0] } ||= &_package_object( (caller)[0] ));
  return $it->{'Channel'};
}

sub Octave_r {
  my($it) = (ref($_[0]) eq "MIDI::Simple") ? (shift @_)
    : ($package{ (caller)[0] } ||= &_package_object( (caller)[0] ));
  return $it->{'Octave'};
}

sub Tempo_r {
  my($it) = (ref($_[0]) eq "MIDI::Simple") ? (shift @_)
    : ($package{ (caller)[0] } ||= &_package_object( (caller)[0] ));
  return $it->{'Tempo'};
}

sub Notes_r {
  my($it) = (ref($_[0]) eq "MIDI::Simple") ? (shift @_)
    : ($package{ (caller)[0] } ||= &_package_object( (caller)[0] ));
  return $it->{'Notes'};
}

sub Volume_r {
  my($it) = (ref($_[0]) eq "MIDI::Simple") ? (shift @_)
    : ($package{ (caller)[0] } ||= &_package_object( (caller)[0] ));
  return $it->{'Volume'};
}

sub Cookies_r {
  my($it) = (ref($_[0]) eq "MIDI::Simple") ? (shift @_)
    : ($package{ (caller)[0] } ||= &_package_object( (caller)[0] ));
  return $it->{'Cookies'};
}

###########################################################################
###########################################################################

=head2 MIDI EVENT ROUTINES

These routines, below, add a MIDI event to the Score, with a
start-time of Time.  Example:

  text_event "And now the bongos!";  # procedure use
  
  $obj->text_event "And now the bongos!";  # method use

These are named after the MIDI events they add to the score, so see
L<MIDI::Event> for an explanation of what the data types (like
"velocity" or "pitch_wheel").  I've reordered this list so that what I
guess are the most important ones are toward the top:


=over

=item patch_change I<channel>, I<patch>;

=item key_after_touch I<channel>, I<note>, I<velocity>;

=item channel_after_touch I<channel>, I<velocity>;

=item control_change I<channel>, I<controller(0-127)>, I<value(0-127)>;

=item pitch_wheel_change I<channel>, I<pitch_wheel>;

=item set_tempo I<tempo>;

=item smpte_offset I<hr>, I<mn>, I<se>, I<fr>, I<ff>;

=item time_signature I<nn>, I<dd>, I<cc>, I<bb>;

=item key_signature I<sf>, I<mi>;

=item text_event I<text>;

=item copyright_text_event I<text>;

=item track_name I<text>;

=item instrument_name I<text>;

=item lyric I<text>;

=item set_sequence_number I<sequence>;

=item marker I<text>;

=item cue_point I<text>;

=item sequencer_specific I<raw>;

=item sysex_f0 I<raw>;

=item sysex_f7 I<raw>;

=back


And here's the ones I'll be surprised if anyone ever uses:

=over

=item text_event_08 I<text>;

=item text_event_09 I<text>;

=item text_event_0a I<text>;

=item text_event_0b I<text>;

=item text_event_0c I<text>;

=item text_event_0d I<text>;

=item text_event_0e I<text>;

=item text_event_0f I<text>;

=item raw_meta_event I<command>(0-255), I<raw>;

=item song_position I<starttime>;

=item song_select I<thing>;

=item tune_request I<starttime>;

=item raw_data I<raw>;

=item end_track I<starttime>;

=item note I<duration>, I<channel>, I<note>, I<velocity>;

=back

=cut

sub key_after_touch ($$$) { &_common_push('key_after_touch', @_) }
sub control_change ($$$) { &_common_push('control_change', @_) }
sub patch_change ($$) { &_common_push('patch_change', @_) }
sub channel_after_touch ($$) { &_common_push('channel_after_touch', @_) }
sub pitch_wheel_change ($$) { &_common_push('pitch_wheel_change', @_) }
sub set_sequence_number ($) { &_common_push('set_sequence_number', @_) }
sub text_event ($) { &_common_push('text_event', @_) }
sub copyright_text_event ($) { &_common_push('copyright_text_event', @_) }
sub track_name ($) { &_common_push('track_name', @_) }
sub instrument_name ($) { &_common_push('instrument_name', @_) }
sub lyric ($) { &_common_push('lyric', @_) }
sub marker ($) { &_common_push('marker', @_) }
sub cue_point ($) { &_common_push('cue_point', @_) }
sub text_event_08 ($) { &_common_push('text_event_08', @_) }
sub text_event_09 ($) { &_common_push('text_event_09', @_) }
sub text_event_0a ($) { &_common_push('text_event_0a', @_) }
sub text_event_0b ($) { &_common_push('text_event_0b', @_) }
sub text_event_0c ($) { &_common_push('text_event_0c', @_) }
sub text_event_0d ($) { &_common_push('text_event_0d', @_) }
sub text_event_0e ($) { &_common_push('text_event_0e', @_) }
sub text_event_0f ($) { &_common_push('text_event_0f', @_) }
sub end_track ($) { &_common_push('end_track', @_) }
sub set_tempo ($) { &_common_push('set_tempo', @_) }
sub smpte_offset ($$$$$) { &_common_push('smpte_offset', @_) }
sub time_signature ($$$$) { &_common_push('time_signature', @_) }
sub key_signature ($$) { &_common_push('key_signature', @_) }
sub sequencer_specific ($) { &_common_push('sequencer_specific', @_) }
sub raw_meta_event ($$) { &_common_push('raw_meta_event', @_) }
sub sysex_f0 ($) { &_common_push('sysex_f0', @_) }
sub sysex_f7 ($) { &_common_push('sysex_f7', @_) }
sub song_position () { &_common_push('song_position', @_) }
sub song_select ($) { &_common_push('song_select', @_) }
sub tune_request () { &_common_push('tune_request', @_) }
sub raw_data ($) { &_common_push('raw_data', @_) }

sub _common_push {
  # I'm your doctor when you need / Have some coke
  # / Want some weed / I'm Your Pusher Man
  #print "*", map("<$_>", @_), "\n";
  my(@p) = @_;
  my $event = shift @p;
  my $it;
  if(ref($p[0]) eq "MIDI::Simple") {
    $it = shift @p;
  } else {
    $it = ($package{ (caller(1))[0] } ||= &_package_object( (caller(1))[0] ) );
  }
  #print "**", map("<$_>", @p), " from ", ()[0], "\n";

  #printf "Pushee to %s 's %s: e<%s>, t<%s>, p<%s>\n",
  #       $it, $it->{'Score'}, $event, ${$it->{'Time'}}, join("~", @p);
  push @{$it->{'Score'}},
    [ $event, ${$it->{'Time'}}, @p ];
  return;
}

###########################################################################
###########################################################################

=head2 MORE ROUTINES

=over

=cut

sub _test_proc {
  my($am_method, $it) = (ref($_[0]) eq "MIDI::Simple")
    ? (1, shift @_)
    : (0, ($package{ (caller)[0] } ||= &_package_object( (caller)[0] )) );
  print " am method: $am_method\n it: $it\n params: <", join(',',@_), ">\n";
}

###########################################################################

=item $opus = write_score I<filespec>

=item $opus = $obj->write_score(I<filespec>)

Writes the score to the filespec (e.g, "../../samples/funk2.midi", or
a variable containing that value), with the score's Ticks as its tick
parameters (AKA "divisions").  Among other things, this function calls
the function C<make_opus>, below, and if you capture the output of
write_score, you'll get the opus created, if you want it for anything.
(Also: you can also use a filehandle-reference instead of the
filespec: C<write_score *STDOUT{IO}>.)

=cut

sub write_score {
  my($am_method, $it) = (ref($_[0]) eq "MIDI::Simple")
    ? (1, shift @_)
    : (0, ($package{ (caller)[0] } ||= &_package_object( (caller)[0] )) );
  my($out, $ticks, $score_r) =
    ( $_[0], (${$it->{'Tempo'}} || 96), $it->{'Score'} );

  croak "First parameter to MIDI::Simple::write_score can't be null\n"
    unless( ref($out) || length($out) );
  croak "Ticks can't be 0" unless $ticks;

  carp "Writing a score with no notes!" unless @$score_r;
  my $opus = $it->make_opus;
# $opus->dump( { 'dump_tracks' => 1 } );

  if(ref($out)) {
    $opus->write_to_handle($out);
  } else {
    $opus->write_to_file($out);
  }
  return $opus; # capture it if you want it.
}

###########################################################################

=item read_score I<filespec>

=item $obj = MIDI::Simple->read_score('foo.mid'))

In the first case (a procedure call), does C<new_score> to erase and
initialize the object attributes (Score, Octave, etc), then reads from
the file named.  The file named has to be a MIDI file with exactly one
eventful track, or Perl dies.  And in the second case, C<read_score>
acts as a constructor method, returning a new object read from the
file.

Score, Ticks, and Time are all affected:

Score is the event form of all the MIDI events in the MIDI file.
(Note: I<Seriously> deformed MIDI files may confuse the routine that
turns MIDI events into a Score.)

Ticks is set from the ticks setting (AKA "divisions") of the file.

Time is set to the end time of the latest event in the file.

(Also: you can also use a filehandle-reference instead of the
filespec: C<read_score *STDIN{IO}>.)

If ever you have to make a Score out of a single track from a
I<multitrack> file, read the file into an $opus, consider something like:

        $opus = MIDI::Opus->new({ 'from_file' => "foo2.mid" });
        $track = ($opus->tracks)[2]; # get the third track
        
        $score_r, $end_time =
          MIDI::Score::events_r_to_score_r($track->events_r);

        $Ticks = $opus->ticks;
        @Score =  @$score_r;
        $Time = $end_time;

=cut

sub read_score {
  my $am_cons = ($_[0] eq "MIDI::Simple");
  shift @_ if $am_cons;

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
  #my $score_r =  ( MIDI::Score::events_r_to_score_r($track->events_r) )[0];
  my( $score_r, $time) = MIDI::Score::events_r_to_score_r($track->events_r);
  #print scalar(@$score_r), " notes in score\n";

  my $it;
  if($am_cons) { # just make a new object and return it.
    $it = MIDI::Simple->new_score;
    $it->{'Score'} = $score_r;
  } else { # need to fudge it back into the pobj
    my $cpackage = (caller)[0];
    #print "~ read_score as a proc for package $cpackage\n";
    if( ref($package{ $cpackage }) ) {  # Already exists in %package
      print "~  reinitting pobj $cpackage\n" if $Debug;
      &_init_score(  $it = $package{ $cpackage }  );
      # no need to call _package_object
    } else {  # Doesn't exist in %package
      print "~  new pobj $cpackage\n" if $Debug;
      $package{ $cpackage } = $it = &_package_object( $cpackage );
      # no need to call _init_score
    }
    @{$it->{'Score'}} = @$score_r;
  }
  ${$it->{'Tempo'}} = $opus->ticks;
  ${$it->{'Time'}} = $time;

  return $it;
}
###########################################################################

=item synch( LIST of coderefs )

=item $obj->synch( LIST of coderefs )

LIST is a list of coderefs (whether as a series of anonymous subs, or
as a list of items like C<(\&foo, \&bar, \&baz)>, or a mixture of
both) that C<synch> calls in order to add to the given object -- which
in the first form is the package object, and which in the second case
is C<$obj>.  What C<synch> does is:

* remember Time before calling any of the routines;

* for each routine given, reset Time to what it was initially, call
the routine, and then note what Time is then;

* then, after having called all of the routines, set Time to whatever
the greatest (latest) value of it resulting from any of the routines.

The coderefs are all called with one argument in C<@_> -- the object
they are supposed to affect.  All these routines should therefore use
methods instead of procedures.  Here's an example usage of synch:

        my $measure = 0;
        my @phrases =(
          [ Cs, F,  Ds, Gs_d1 ], [Cs,    Ds, F, Cs],
          [ F,  Cs, Ds, Gs_d1 ], [Gs_d1, Ds, F, Cs]
        );
        
        for(1 .. 20) { synch(\&count, \&lalala); }
        
        sub count {
          my $it = $_[0];  $it->r(wn); # whole rest
          ++$measure;
        }
        
        sub lalala {
          my $it = $_[0];
          $it->noop(c1,mf,o3,qn); # setup
          my $phrase_number = ($measure + -1) % 4;
          my @phrase = @{$phrases[$phrase_number]};
          foreach my $note (@phrase) { $it->n($note); }
        }

=cut

sub synch {
  my($am_method, $it) = (ref($_[0]) eq "MIDI::Simple")
    ? (1, shift @_)
    : (0, ($package{ (caller)[0] } ||= &_package_object( (caller)[0] )) );

  my @subs = grep(ref($_) eq 'CODE', @_);

  print " My subs: ", map("<$_> ", @subs), ".\n"
   if $Debug;
  return unless @subs;
  # my @end_times = (); # I am the Lone Array of the Apocalypse!
  my $orig_time = ${$it->{'Time'}};
  my $max_time  = $orig_time;
  foreach my $sub (@subs) {
    printf " Before %s\:  Entry time: %s   Score items: %s\n",
            $sub, $orig_time, scalar(@{$it->{'Score'}}) if $Debug;
    ${$it->{'Time'}} = $orig_time; # reset Time

    &{$sub}($it); # now call it

    printf "   %s items ending at %s\n",
     scalar( @{$it->{'Score'}} ), ${$it->{'Time'}} if $Debug;
    $max_time = ${$it->{'Time'}} if ${$it->{'Time'}} > $max_time;
  }
  print " max end-time of subs: $max_time\n" if $Debug;

  # now update and get out
  ${$it->{'Time'}} = $max_time;
}

########################################################################### 

=item $opus = make_opus  or  $opus = $obj->make_opus

Makes an opus (a MIDI::Opus object) out of Score, setting the opus's
tick parameter (AKA "divisions") to $ticks.  The opus is,
incidentally, format 0, with one track.

=cut

sub make_opus {
  # Make a format-0 one-track MIDI out of this score.

  my($am_method, $it) = (ref($_[0]) eq "MIDI::Simple")
    ? (1, shift @_)
    : (0, ($package{ (caller)[0] } ||= &_package_object( (caller)[0] )) );

  my($ticks, $score_r) = (${$it->{'Tempo'}}, $it->{'Score'});
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

=item dump_score  or  $obj->dump_score

Dumps Score's contents, via C<print> (so you can C<select> an output
handle for it).  Currently this is in this kind of uninspiring format:

  ['note', 0, 96, 1, 25, 96],
  ['note', 96, 96, 1, 29, 96],

as it is (currently) just a call to &MIDI::Score::dump_score; but in
the future I may (should?) make it output in C<n>/C<r> notation.  In
the meantime I assume you'll use this, if at all, only for debugging
purposes.

=cut

sub dump_score {
  my($am_method, $it) = (ref($_[0]) eq "MIDI::Simple")
    ? (1, shift @_)
    : (0, ($package{ (caller)[0] } ||= &_package_object( (caller)[0] )) );
  return &MIDI::Score::dump_score( $it->{'Score'} );
}

###########################################################################
###########################################################################

=back

=head2 FUNCTIONS

These are subroutines that aren't methods and don't affect anything
(i.e., don't have "side effects") -- they just take input and/or give
output.

=over

=item interval LISTREF, LIST

This takes a reference to a list of integers, and a list of note-pitch
specifications (whether relative or absolute), and returns a list
consisting of the given note specifications transposed by that many
half-steps.  E.g.,

  @majors = interval [0,4,7], C, Bflat3;

which returns the list C<(C,E,G,Bf3,D4,F4)>.

=cut

sub interval { # apply an interval to a list of notes.
  my(@out);
  my($am_method, $it) = (ref($_[0]) eq "MIDI::Simple")
    ? (1, shift @_)
    : (0, ($package{ (caller)[0] } ||= &_package_object( (caller)[0] )) );
  my($interval_r, @notes) = @_;

  croak "first argument to &MIDI::Simple::interval must be a listref\n"
   unless ref($interval_r);
  # or a valid key into a hash %Interval?

  foreach my $note (@notes) {
    my(@them, @status, $a_flag, $note_number);
    @status = &is_note_spec($note);
    next unless @status;

    ($a_flag, $note_number) = @status;
    @them = map { $note_number + $_ } @$interval_r;

    if($a_flag) { # If based on an absolute note spec.
      @them = map(&number_to_absolute($_), @them)
        unless $note =~ m<^n\d+$>s;  # keep it numeric
    } else { # If based on a relative note spec.
      @them = map(&number_to_relative($_), @them);
    }
    push @out, @them;
  }
  return @out;
}
#--------------------------------------------------------------------------

=item note_map { BLOCK } ( ... LIST ... )

This is pretty much based on the normal C<map()> function, altho the
syntax is a bit more restrictive.

BLOCK is a segment of code to apply 
LIST is a list of note-pitch specifications

=cut

sub note_map (&@) { # map a function to a list of notes
  my($sub, @notes) = @_;
  return() unless @notes;

  return
    map {
      my($note, @them, @status, $a_flag, $note_number) = $_;
      @status = &is_note_spec($note);
      if(@status) {
        ($a_flag, $note_number) = @status;
        @them = map { &{$sub}($_, $note_number, $a_flag, $note ) }
                    $note_number;

        if($a_flag) { # If based on an absolute note spec.
          @them = map(&number_to_absolute($_), @them)
            unless $note =~ m<^n\d+$>s; # keep it numeric
        } else { # If based on a relative note spec.
          @them = map(&number_to_relative($_), @them);
        }
      }
      @them;
    }
  @notes
  ;
}

###########################################################################

=item number_to_relative

=cut

sub number_to_relative ($) {
  my $o_spec;
  my $in = int($_[0]);

  if($in < 0) { # Negative, so 'octave(s) down'
    $o_spec = '_d' . (1 + abs(int(($in + 1) / 12)));  # Crufty, but it works.
  } elsif($in < 12) {  # so 'same octave'
    $o_spec = '';
  } else {  # Positive, greater than 12, so 'N octave(s) up'
    $o_spec = '_u' . int($in / 12);
  }
  return( $MIDI::Simple::Note[ $in % 12 ] . $o_spec );
}

=item number_to_absolute

=cut

sub number_to_absolute ($) {
  my $in = int($_[0]);
  return( $MIDI::Simple::Note[ $in % 12 ] . int($in / 12) );
}

###########################################################################

=item is_note_spec(STRING)

=cut

sub is_note_spec ($) {
  # if false, return()
  # if true,  return(absoluteness_flag, $note_number)
  my($in, @ret) = ($_[0]);
  return(@ret) unless length $in;
  @ret = &is_absolute_note_spec($in);  return(1, @ret) if @ret;
  @ret = &is_relative_note_spec($in);  return(0, @ret) if @ret;
  return(@ret);
}

=item is_relative_note_spec(STRING)

=cut

sub is_relative_note_spec ($) {
  # if false, return()
  # if true,  return($note_number)
  my($note_number, $octave_number, $in, @ret) = (-1, 0, $_[0]);
  return() unless length $in;

  if($in =~ m<^([A-Za-z]+)$>s   # Cs
     and exists( $MIDI::Simple::Note{$1} )
  ){
    $note_number = $MIDI::Simple::Note{$1};
  } elsif($in =~ m<^([A-Za-z]+)_([du])(\d+)$>s   # Cs_d4, Cs_u1
     and exists( $MIDI::Simple::Note{$1} )
  ){
    $note_number = $MIDI::Simple::Note{$1};
    $octave_number = $3;
    $octave_number *= -1  if $2 eq "d";
  } else {
    @ret = ();
  }
  unless($note_number == -1) {
    @ret = ( $note_number + $octave_number * 12 );
  }
  return @ret;
}

=item is_absolute_note_spec(STRING)

=cut

sub is_absolute_note_spec ($) {
  # if false, return()
  # if true,  return($note_number)
  my($note_number, $in, @ret) = (-1, $_[0]);
  return() unless length $in;
  if( $in =~ /^n?(\d+)$/s ) {  # E.g.,  "29", "n38"
    $note_number = 0 + $1;
  } elsif( $in =~ /^([A-Za-z]+)(\d+)/s ) {  # E.g.,  "C3", "As4"
    $note_number = $MIDI::Simple::Note{$1} + $2 * 12
      if exists($MIDI::Simple::Note{$1});
  }
  @ret = ($note_number) if( $note_number >= 0 and $note_number < 128);
  return @ret;
}

#--------------------------------------------------------------------------

=item Self() or $obj->Self();

=cut

sub Self { # pointless as a method -- but as a sub, useful if
  # you want to access your current package's object.
  # Juuuuuust in case you need it.
  my($am_method, $it) = (ref($_[0]) eq "MIDI::Simple")
    ? (1, shift @_)
    : (0, ($package{ (caller)[0] } ||= &_package_object( (caller)[0] )) );
  return $it;
}

=back

=cut

###########################################################################

=head1 AUTHOR

Sean M. Burke C<sburke@netadventure.net>

=cut

1;

__END__
