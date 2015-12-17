\version "2.18"

\include "../roman_numeral_analysis_tool.ily"

bass = \relative c {
  \clef bass
  \key es \major
  \time 3/4
  des16 bes' es, bes' c, aes' es aes ces, a' es a
  bes,16 bes' f d bes2
  \bar "||"
}

analysis = \lyricmode {
  \set stanza = \markup \keyIndication { Eflat } %% after 2.19.15 use E-flat
  \override LyricText.self-alignment-X = #LEFT
  \markup \rN { V 4 2 / IV }4
  \markup \rN { IV 6 }
  \markup \rN { It +6 }
  \markup \rN { V }2.
  % Equivalent to the following:
  % V2.
}

\new Staff <<
  \new Voice \bass
  \new Lyrics \analysis
>>
