\version "2.18"

\include "../roman_numeral_analysis_tool.ily"

bassline = \relative c' {
  \clef bass
  \key g \major
  \time 3/4
  g4 fis f
  e es es
  d2 dis4
  e2.
  \bar "||"
}

analysis = \lyricmode {
  \override LyricText.self-alignment-X = #-0.6
  \offset StanzaNumber.X-offset #-3
  \set stanza  = #"G:"
  % For bare Roman numerals, \rN simply outputs the string.
  %\markup \rN { I }
  I
  \markup \rN { V 6 5 }
  \markup \rN { vii o 4 3 / IV }
  \markup \rN { IV 6 }
  \markup \rN { ii h 4 3 }
  \markup \rN { Fr +6 }
  \markup \rN { I 6 4 }
  \markup \rN { vii o 7 / vi }
  vi
}

\score {
  \new Staff <<
    \new Voice = "bass" { \bassline }
    \new Lyrics \with {
      % to control distance of analysis from staff
      \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 6.5))
    } \lyricsto "bass" { \analysis }
  >>
  \layout {
    \context {
      \Score
      % to control horizontal spacing
      \override SpacingSpanner.shortest-duration-space = #6
    }
    \context {
      \Lyrics
      % to control global size
      %\override LyricText.font-size = #-1
    }
  }
}
