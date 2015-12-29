LilyPond Roman numeral analysis tool
====================================

## What's this?

This tool provides a convenient method of creating Roman numerals for harmonic
analysis.  It works in conjunction with the program GNU LilyPond, which is
available for download [here](http://lilypond.org/download.html).

## How is it used?

To make this tool available to your LilyPond file, you can either

1. copy the code in `roman_numeral_analysis_tool.ily` directly into your `ly` file, or
2. add `\include "roman_numeral_analysis_tool.ily"` to the head (assuming
that both files are in the same directory).

The two methods do the same thing, but using `include` is a good way to keep your `.ly`
file uncluttered.

Roman numerals may be created wherever markups are allowed within a LilyPond
file.  When adding an analysis to music, however, it is strongly recommended
that you create your analysis within a `Lyrics` context.

Symbols are created by the `rN` command, for which the syntax is as follows:

`\markup \rN { ...list of symbols... }`

Between the curly brackets, list the needed components of a single Roman numeral in
the following order:

1. Roman numeral or note name of root;
2. superscript quality indicator;
3. figures from top to bottom.
4. For a secondary function, continue with `/` followed by
5. tonicized Roman numeral or note name of root.

Usually, you can skip unnecessary items, though including an empty string
(`""`) as a spacer may be needed in some ambiguous cases.  Components must be
separated by whitespace.

As a simple example, the notation

`markup \rN { V }`

outputs a Roman numeral for a dominant triad.

(There is little point in using `\rN` for such trivial examples, except as a
reminder perhaps of the purpose of the markup.  No special formatting is added
in the case of an unadorned Roman numeral (i.e., without quality indicator,
figures, etc.)  The notation `\markup { V }` or `V` within a `lyricmode`
expression yields the same result.)

### Quality

The following special indicators for chord quality are available: `o` for
diminished, `h` for half-diminished, and `+` for augmented.  Other possibilities
include combinations of `M` and `m` (`M`, `m`, `MM7`, `Mm`, `mm`, `Mmm9`, etc.);
`add`, `add6`, etc.

An augmented dominant triad is produced like so:

`\markup \rN { V + }`

### Figures

List figures in top-to-bottom order.  The following yields a first-inversion
V7 chord; note that there is no need to account for the absent quality
indicator:

`\markup \rN { V 6 5 }`

More than two figures are possible:

`\markup \rN { V 6 5 3 }`

Isolated figures are possible, too.  (If you want to engrave figured bass,
however, you should use LilyPond's native support.)

`\markup \rN { 4 2 }`

Blank space within figures requires quoting:

`\markup \rN { I "5   6   6" "3   3   4" }`

### Secondary functions

For secondary functions, simply add the slash and secondary root.  For example,
the notation

`\markup \rN { vii o 4 3 / V }`

produces a secondary leading-tone diminished-seventh chord of V.

### Alterations

Raising or lowering a chord root may be indicated by prepending alteration
symbols to the Roman numeral, with no intervening spaces.

To accommodate analytical styles which indicate triad quality by an accidental
placed after a chord root, alteration symbols may be added as suffixes.

(Adding both an accidental suffix and prefix is currently not possible.)

The symbols for alterations are derived from LilyPond's English input language.
These are `flat` or `f`; `sharp` or `s`; `natural` or `n`.  Though there will
be little occasion to add double flats or double sharps, you can do it with
`flatflat` or `ff`; and `sharpsharp`, `x`, or `ss`.

As an example of altered-chord entry, a Neapolitan-sixth would be indicated
like so:

`\markup \rN { fII 6 }`

or

`\markup \rN { flatII 6 }`

Alterations to figures may be created in similar fashion, by adding various
prefixes or suffixes.  The indicators available are as follows: `flat`, `f`,
`sharp`, `s`, `+`, `natural` and `n`.  So, for example, to represent a
dominant-seventh chord with raised 5th, you would write

`\markup \rN { V 7 +5 }`

Alterations may appear anywhere within a figure.  So you may write

`\markup \rN { I 5--f6--5 }`

Alterations may appear *alone* in figures; currently, however, for technical
reasons, this is not possible in the topmost figure.

### Lines

Hyphens in figures are converted to en-dashes.  It is possible to simulate
extender lines by using multiple hyphens:

`\markup \rN { I 5---6---5 3---4---3 }`

You can decide if these join well enough for your purposes!

### Note names

This tool is designed as a typesetter for Roman numeral analysis, not for
lead-sheet notation.  There may, however, be occasion to use note names to
represent roots (and certainly for key indications, covered below). Limited
support for English note names is therefore available.

(If you require lead-sheet notation, LilyPond proper has extensive support.)

To indicate a note name, simply write the name used for English input in
LilyPond.  You may capitalize the first letter for a capital in the output.

So to indicate an A-sharp, you may write:

`\markup \rN { As }`

You can also use the longer name.  In versions after 2.19.15, you would write

`\markup \rN { A-sharp }`

and for versions before

`\markup \rN { Asharp }`

Note names may be combined with Roman numerals.  For example,

`\markup \rN { V 7 / B-flat }`

Note that in all examples shown, the `rN` command is preceded by `\markup`.  As
we produce Roman numerals as LilyPond markups, this is unavoidable.

### Changing defaults

The appearance of Roman numerals may be changed in a host of ways common to
other such objects in LilyPond.  It is beyond the scope of this document to
cover these, but mention will be made of the importance of the *context* in
which the notation was created.

Earlier it was recommended that you use a `Lyrics` context when applying
Roman-numeral analysis to musical passages.  If you wanted to scale your
numerals by changing the font size, you would then be modifying `LyricText`
items:

`\override Lyrics.LyricText.font-size = #2`

If you created the markup outside of a `Lyrics` context, for example, within
a paragraph of text, you would need to override `TextScript` items.  The
following would change the size of a single such item:

`\once \override TextScript.font-size = #2`

Another way to scale a single numeral would be within the actual markup
command invocation:

`\markup \override #'(font-size . 2) \rN { ... }`

### Key Indications

It can be a hassle to tweak markup commands to produce nice-looking key
indications when sharps and flats are involved.  The `\rN` command eases
this through its note-name syntax.

Key indications are easily created in a `Lyrics` context by setting stanza
numbers to the output of `\rN`.

`\set stanza = \markup { \concat { \rN { B-flat } \hspace #0.2 : } }`

The above produces the typical key-followed-by-colon notation, which is
made easier by the convenience function `\keyIndication`:

`\set stanza = \markup \keyIndication { B-flat }`

To change the position of the key indication you may write

`\override StanzaNumber.X-offset = #-5`

or (more predictably)

`\offset StanzaNumber.X-offset #-1`

Alternately, you can add spaces after the input string like so:

`\set stanza = \markup \keyIndication { "b-flat     " }`

(Notice the lowercase note name indicating a minor key.)

This method, however, may require an additional override for good spacing:

`\override StanzaNumber.self-alignment-X = #RIGHT`

The `\keyIndication` command may be used independently of `StanzaNumber` and
a `Lyrics` context, in which case the item to modify will be a `TextScript`
item.

### Scale degrees

Scale degree indications can be created using the `\scaleDegree` markup
command.  The output is a number with caret preceded by an optional
accidental.  Any of the English accidental prefixes may be used.  To produce
notation for scale degree 5, you would write

`\markup \scaleDegree { 5 }`

To indicate a raised scale degree 2, you would write

`\markup \scaleDegree { s2 }`

or

`\markup \scaleDegree { sharp2 }`

Note: To cut down on typing, you may want to rename this command and
`\keyIndication` above.

## Complete examples

The directory `examples` contains several illustrations of usage.
