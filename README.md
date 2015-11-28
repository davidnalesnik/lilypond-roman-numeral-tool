LilyPond Roman numeral analysis tool
====================================

## What's this?

This tool provides a convenient method of creating Roman numerals for harmonic
analysis.  It works in conjunction with the program GNU LilyPond, which is
available for download [here](http://lilypond.org/download.html).

## How is it used?

To make this tool available to your LilyPond file, you can either copy the code
in `roman_numeral_analysis_tool.ily` directly into your `ly` file, or you can add
`\include "roman_numeral_analysis_tool.ily"` to the head (assuming
that both files are in the same directory).  The two methods do the same thing,
but using `include` is a good way to keep your `.ly` file uncluttered.

Roman numerals may be created wherever markups are allowed.  When adding an
analysis to music, however, it is strongly recommended that you create your
analysis within a `Lyrics` context.

Symbols are created by the `rN` command, for which the syntax is as follows:

`\markup \rN { ...list of symbols... }`

Between the curly brackets, list the components of a single Roman numeral in
the following order (as needed): Roman numeral or note-name, quality,
figured-bass numbers from top to bottom, `/` (for a secondary function),
tonicized Roman numeral or note-name.  Usually, you can skip unnecessary items,
though including an empty string (`""`) as a spacer may be needed in some
ambiguous cases.  Components must be separated by whitespace.

To indicate a note-name, simply write the name used for English input in
LilyPond.  You may capitalize the first letter for a capital in the output.
To put an alteration before a Roman numeral, prefix the numeral
without space with the LilyPond English alteration name: `f`, `flat`, `s`,
`sharp`, `s`, `x`, `sharpsharp`, `n`, `natural`.  Examples: `fVII` &#x2192;
"flat VII"; `sharpvi` &#x2192; "sharp vi".

These symbols may also precede inversion figures: `f6`, `s11`, etc.  Use of
`+` for a raised figure is not currently supported.

At the moment, only English naming is supported.  It is possible to modify
the code to accomodate other languages.

Qualities are indicated by `o` for diminished, `h` for half-diminished,
`+` for augmented, and `f` for flat.  Other possibilities include
combinations of `M` and `m` (`M`, `m`, `MM7`, `Mm`, `mm`, `Mmm9`, etc.);
`add`, `add6`, etc.

To scale all numerals: `\override LyricText #'font-size = #2`
or `\override TextScript #'font-size = #2`

To scale individual numerals:
`\markup \override #'(font-size . 2) \rN { ... }`

### Key Indications

Key indications are easily created in a `Lyrics` context using stanza
numbers.  The `\rN` command makes it easy to create note-names with sharps
or flats.  To produce the typical key-followed-by-colon notation, you can use
the markup command `\keyIndication` like so:

`\set stanza = \markup \keyIndication { B-flat }`

To change the position of the key indication you may write

`\override StanzaNumber.X-offset = #-5`

or

`\offset StanzaNumber.X-offset #-1`

Alternately, you can add spaces after the input string like so:

`\set stanza = \markup \keyIndication { "b-flat     " }`

This method, however, may require an additional override for good spacing:

`\override StanzaNumber.self-alignment-X = #RIGHT`

## Example

The file `example.ly` gives an illustration of usage.
