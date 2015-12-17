\version "2.18"

\include "../roman_numeral_analysis_tool.ily"

scale = \relative c' {
  \key es \major
  es4 e f fis
  g aes a bes
  b c cis d
  es4 d des c
  ces bes beses aes
  g ges f fes
  es1
  \bar "||"
}

scaleDegrees = \lyricmode {
  \markup \scaleDegree { 1 }4
  \markup \scaleDegree { sharp1 }
  \markup \scaleDegree { 2 }
  \markup \scaleDegree { s2 }
  \markup \scaleDegree { 3 }
  \markup \scaleDegree { 4 }
  \markup \scaleDegree { s4 }
  \markup \scaleDegree { 5 }
  \markup \scaleDegree { s5 }
  \markup \scaleDegree { 6 }
  \markup \scaleDegree { s6 }
  \markup \scaleDegree { 7 }
  \markup \scaleDegree { 1 }
  \markup \scaleDegree { 7 }
  \markup \scaleDegree { flat7 }
  \markup \scaleDegree { 6 }
  \markup \scaleDegree { f6 }
  \markup \scaleDegree { 5 }
  \markup \scaleDegree { f5 }
  \markup \scaleDegree { 4 }
  \markup \scaleDegree { 3 }
  \markup \scaleDegree { f3 }
  \markup \scaleDegree { 2 }
  \markup \scaleDegree { f2 }
  \markup \scaleDegree { 1 }1
}

noteNames = \lyricmode {
  \offset StanzaNumber.X-offset #-3
  \set stanza = \markup \keyIndication { Eflat }
  \markup \rN { Ef }4
  \markup \rN { En }
  \markup \rN { F }
  \markup \rN { Fs }
  \markup \rN { G }
  \markup \rN { Af }
  \markup \rN { An }
  \markup \rN { Bf }
  \markup \rN { Bn }
  \markup \rN { C }
  \markup \rN { Cs }
  \markup \rN { D }
  \markup \rN { Ef }
  \markup \rN { D }
  \markup \rN { Df }
  \markup \rN { C }
  \markup \rN { Cf }
  \markup \rN { Bf }
  \markup \rN { Bff }
  \markup \rN { Af }
  \markup \rN { G }
  \markup \rN { Gf }
  \markup \rN { F }
  \markup \rN { Ff }
  \markup \rN { Ef }1
}

\score {
  <<
    \new Staff = "staff" {
      \new Voice \scale
    }
    \new Lyrics \with {
      alignAboveContext = "staff"
    } \scaleDegrees
    \new Lyrics \with {
      % to control distance of analysis from staff
      \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 6.5))
    } \noteNames
  >>
  \layout {
    \context {
      \Lyrics
      \override LyricText.font-size = #0
    }
  }
}