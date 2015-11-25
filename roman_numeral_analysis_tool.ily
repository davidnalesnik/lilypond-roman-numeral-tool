\version "2.18"

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% A function to create Roman numerals for harmonic analysis.
%%
%% Syntax: \markup \rN { ...list of symbols... }
%%
%% List symbols in this order (as needed): Roman numeral (or note-name),
%% quality, top number of inversion symbol, bottom number, "/" (if secondary
%% function), Roman numeral (or note-name).  Usually, you can skip unnecessary
%% items, though a spacer may be needed in some cases.  Use "" instead of the
%% initial symbol to start with the quality or inversion, for example.
%%
%% Preceding or following a symbol with English alterations (f, s, ff, ss, x, n)
%% will attach accidentals: "fVII" -> flat VII; "svi" -> sharp vi; "Af" -> A-flat;
%% "As" A-sharp
%%
%% Qualities: use "o" for diminished, "h" for half-diminished,
%% "+" for augmented, "f" for flat; other indications are possible such as
%% combinations of "M" and "m" (M, m, MM7, Mm, mm, Mmm9, etc.); add, add6, etc.
%%
%% To scale all numerals: \override  LyricText #'font-size = #2
%% or \override  TextScript #'font-size = #2
%% To scale individual numerals: \markup \override #'(font-size . 2) \rN { ... }
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% INPUT FORMATTING %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% Split a list of strings by a splitter which is a member of a list of
%% potential splitters.  To split by "f6" or "+6" based on the list of numbers
%% below, the splitter need only be part of a string.
%% input is split into (( ...up to splitter... ) ( ...beginning with splitter... ))
%% Used to split notation for secondary chords and to isolate inversion numbers
#(define (split-list symbols splitter-list)
   (let loop ((sym symbols) (result '()))
     (cond
      ((or (null? sym)
           (find (lambda (y) (string-contains (car sym) y)) splitter-list))
       (list (reverse result) sym))
      (else (loop (cdr sym) (cons (car sym) result))))))

#(define numbers '("2" "3" "4" "5" "6" "7" "8" "9" "11" "13"))

#(define qualities
   ;; only to allow omission of base when quality is alone
   ;; TODO--combinations of M and m, add, ADD . . .
   '("o" "+" "h"))

#(define (base-and-quality arg)
   (let ((len (length arg)))
     (cond
      ((= 0 len) '(() ()))
      ((= 1 len)
       (if (find (lambda (y) (string= (car arg) y)) qualities)
           (list '() (list (car arg)))
           (list (list (car arg)) '()))) ;; TODO figure out which is given
      ((= 2 len) (list (list (car arg)) (cdr arg))))))

#(define (base-quality-inversion symbols)
   ;; given (vii o 4 3) --> ((vii) (o) (4 3)) with call to base-and-quality
   ;; (4 3) --> (() () (4 3))
   ;; () --> (() () ())
   (let* ((split-by-numbers (split-list symbols numbers))
          (b-and-q (base-and-quality (car split-by-numbers))))
     (append b-and-q (cdr split-by-numbers))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%% NOTE NAMES / ACCIDENTALS %%%%%%%%%%%%%%%%%%%%%%%%%%
%% Based on English names.  For other languages, change the strings
%% in the three following definitions.

#(define notenames "AaBbCcDdEeFfGg")

%% Arranged in descending length so no need to search for longest match.
#(define alterations '("ff" "ss" "f" "s" "x" "n"))

%{
#(define (notename? str)
   (and
    ;; first character is in list of note names
    (number? (string-index str (string->char-set notenames) 0 1))
    ;; All of remaining characters match an entry in alteration list
    (or (= (string-length str) 1)
        (and (find (lambda (s) (string= s (string-drop str 1))) alterations)
             #t))))
%}
#(define (parse-string-with-accidental str)
   "Given @var{str}, return a list in this format: (initial-accidental?
notename-or-figure-or-RN terminal-accidental?) If an accidental is found, include
its string, otherwise @code{#t}."
   (if (not (string-null? str))
       (let* ((first-char (string-take str 1))
              (all-but-first (string-drop str 1))
              (all-but-first (if (string-null? all-but-first) #f all-but-first)))
         ;; Is it a notename with optional accidental?
         (if (and (string-contains notenames first-char)
                  (or (not all-but-first)
                      (find (lambda (s) (string= s all-but-first)) alterations)))
             (list #f first-char all-but-first)
             ;; Is it a Roman numeral or figure preceded by an accidental?
             (let* ((accidental-prefix
                     (find (lambda (s) (string-prefix? s str)) alterations))
                    (rest (if accidental-prefix
                              (string-drop str (string-length accidental-prefix))
                              str)))
               (list accidental-prefix rest #f))))))
%{
#(define (inversion? str)
   "Check to see if a string contains a digit.  If so, it is an inversion figure."
   (not (char-set=
         char-set:empty
         (char-set-intersection (string->char-set str) char-set:digit))))
%}
#(define (big-char? arg) ; offset after awkward characters
   (let ((last-char (string-take-right arg 1)))
     (cond
      ((string= last-char "V") 0.1)
      ((string= last-char "f") 0.2)
      ((string= last-char "s") 0.2) ; sharp
      ((string= last-char "x") 0.2) ; double-sharp
      ((string= last-char "ss") 0.2) ; double-sharp
      (else 0.0))))

#(define (acc size-factor)
   `(("f" . ,(make-raise-markup (* 0.3 size-factor) (make-flat-markup)))
     ("ff" . ,(make-raise-markup (* 0.3 size-factor) (make-doubleflat-markup)))
     ("s" . ,(make-raise-markup (* 0.6 size-factor) (make-sharp-markup)))
     ("ss" . ,(make-raise-markup (* 0.3 size-factor) (make-doublesharp-markup)))
     ("x" . ,(make-raise-markup (* 0.3 size-factor) (make-doublesharp-markup)))
     ("n" . ,(make-raise-markup (* 0.6 size-factor) (make-natural-markup)))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% BASE MARKUP %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#(define (make-base-markup base size)
   (let* ((size-factor (magstep size))
          (base-list (parse-string-with-accidental base))
          (init-acc (first base-list))
          (end-acc (last base-list)))
     (cond
      (init-acc
       (make-concat-markup
        (list (make-fontsize-markup -3 (assoc-ref (acc size-factor) init-acc))
          (make-hspace-markup (* 0.2 size-factor))
          (second base-list))))
      (end-acc
       (make-concat-markup
        (list (second base-list)
          (make-hspace-markup (* size-factor (big-char? (second base-list))))
          (make-hspace-markup (* size-factor 0.2))
          (make-fontsize-markup -3 (assoc-ref (acc size-factor) end-acc)))))
      (else
       (make-concat-markup
        (list base
          (make-hspace-markup (* size-factor
                                (big-char? base)))))))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% QUALITY %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#(define (dim size)
   "Create circle markup for diminished quality."
   (let* ((size-factor (magstep size))
          (r (* 0.3 size-factor))
          (th (* 0.1 size-factor)))
     (make-translate-markup
      (cons r r)
      (make-draw-circle-markup r th #f))))

#(define (half-dim size)
   "Create slashed circle markup for half-diminished quality."
   (let* ((size-factor (magstep size))
          (x (* size-factor 0.35))
          (y (* size-factor 0.35))
          (r (* size-factor 0.3))
          (th (* size-factor 0.1)))
     (make-translate-markup
      (cons x y)
      (make-combine-markup
       (make-draw-circle-markup r th #f)
       (make-override-markup `(thickness . ,size-factor)
         (make-combine-markup
          (make-draw-line-markup (cons (- x) (- y)))
          (make-draw-line-markup (cons x y))))))))

#(define (aug size)
   "Create cross markup for augmented quality."
   (let* ((size-factor (magstep size))
          (x (* size-factor 0.35))
          (y (* size-factor 0.35)))
     (make-override-markup `(thickness . ,size-factor)
       (make-translate-markup (cons x y)
         (make-combine-markup
          (make-combine-markup
           (make-draw-line-markup (cons (- x) 0))
           (make-draw-line-markup (cons 0 (- y))))
          (make-combine-markup
           (make-draw-line-markup (cons x 0))
           (make-draw-line-markup (cons 0 y))))))))

#(define (make-quality-markup size offset quality)
   (cond
    ((string= quality "o") (make-raise-markup (* 1.25 offset) (dim size)))
    ((string= quality "h") (make-raise-markup (* 1.25 offset) (half-dim size)))
    ((string= quality "+") (make-raise-markup (* 1.25 offset) (aug size)))
    ((string= quality "f") (make-raise-markup (* 1.5 offset)
                             (make-fontsize-markup (- size 5)
                               (make-flat-markup))))
    (else (make-raise-markup offset (make-fontsize-markup -3 quality)))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% INVERSION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#(define (format-figures figures size)
   (let ((size-factor (magstep size)))
     (map (lambda (fig)
            (let* ((figure-list (parse-string-with-accidental fig))
                   (init-acc (car figure-list)))
              (cond
               (init-acc
                (make-concat-markup
                 (list
                  (make-fontsize-markup -3 (assoc-ref (acc size-factor) init-acc))
                  (make-hspace-markup (* 0.2 size-factor))
                  (markup (second figure-list)))))
               (else (markup fig)))))
       figures)))

#(define (make-inversion-markup size offset figures)
   (let ((formatted-figures (format-figures figures 1)))
     (make-fontsize-markup -3
       (make-override-markup `(baseline-skip . ,(* 1.4 (magstep size)))
         (make-raise-markup offset
           (make-right-column-markup formatted-figures))))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% SECONDARY RN %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#(define (make-secondary-markup size second-part)
   (make-concat-markup
    (list
     (car second-part)
     (if (string-null? (cadr second-part))
         empty-markup
         (make-concat-markup
          (list
           (make-hspace-markup (* 0.2 (magstep size)))
           (if (car (parse-string-with-accidental (cadr second-part)))
               (make-hspace-markup (* 0.2 (magstep size)))
               empty-markup)
           (make-base-markup (cadr second-part) size)))))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% SYNTHESIS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#(define-markup-command (rN layout props symbols) (markup-list?)
   #:properties ((font-size 1))
   (let* ((split (split-list symbols '("/")))
          (first-part (base-quality-inversion (car split)))
          (second-part (cadr split));(cadr normalized)) ; slash and what follows
          (base (car first-part))
          (quality (cadr first-part))
          (inversion (caddr first-part))
          (base-markup (make-base-markup (car base) font-size))
          (size-factor (magstep font-size))
          ;; height of inversion and quality determined by midline of base
          (dy (* 0.5
                (interval-length
                 (ly:stencil-extent
                  (interpret-markup
                   layout props (if (or (null? base)
                                        (string-null? (car base)))
                                    "/"
                                    base-markup))
                  Y)))))

     (interpret-markup layout props
       (make-concat-markup
        (list
         (if (or (null? base) (string-null? (car base))) ; "" used as spacer
             empty-markup
             (make-concat-markup
              (list
               base-markup
               (make-hspace-markup (* size-factor (big-char? (car base)))))))
         (if (null? quality)
             empty-markup
             (make-concat-markup
              (list
               (make-hspace-markup (* 0.1 size-factor))
               (make-quality-markup font-size dy (car quality)))))
         (if (null? inversion)
             empty-markup
             (make-concat-markup
              (list (make-hspace-markup (* 0.1 size-factor))
                (make-inversion-markup font-size dy inversion))))
         (if (null? second-part)
             empty-markup
             (make-concat-markup
              (list
               (if (= (length inversion) 1)
                   ;; allows slash to tuck under if single inversion figure
                   (make-hspace-markup (* -0.2 size-factor))
                   ;; slightly more space given to slash
                   (make-hspace-markup (* 0.2 size-factor)))
               (make-secondary-markup font-size second-part)))))))))
