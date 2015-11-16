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
  \set stanza = #"G:     " % use spaces to adjust position of key indication
  \markup \rN { I }
  \markup \rN { V 6 5 3 }
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
      \override SpacingSpanner #'shortest-duration-space = #6
    }
  }
}
