\version "2.18"

\include "roman_numeral_analysis_tool.ily"

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
  \markup \rN { I }
  \markup \rN { V 6 5 }
  \markup \rN { vii o 4 3 / IV }
  \markup \rN { IV 6 }
  \markup \rN { ii h 4 3 }
  \markup \rN { Fr + 6 }
  \markup \rN { I 6 4 }
  \markup \rN { vii o 7 / vi }
  \markup \rN { vi }
}

\score {
  \new Staff <<
    \new Voice = "bass" { \bassline }
    \new Lyrics \lyricsto "bass" { \analysis }
  >>
  \layout {
    \context {
      \Score
      % to control line spacing
      \override SpacingSpanner.shortest-duration-space = #6
    }
  }
}
