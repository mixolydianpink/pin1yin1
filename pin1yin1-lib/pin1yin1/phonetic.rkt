#lang racket/base

(provide (struct-out syllable)
         (struct-out polysyllable)
         (struct-out compound)

         syllable-segments/raw
         syllable-pin1yin1
         syllable-pinyin-core
         syllable-zhuyin-core
         syllable-zhuyin-tone-mark)

(require (only-in racket/function
                  disjoin)
         (only-in racket/list
                  empty?)
         racket/match

         pin1yin1/option
         pin1yin1/pst
         pin1yin1/zhupin)

(struct syllable (segments tone erization capitalized?) #:transparent
  #:guard
  (λ (segments tone erization capitalized? name)
    (if (and (and (list? segments)
                  (andmap char? segments)
                  (some? (pst-ref zhupin-pst segments)))
             (member tone '(0 1 2 3 4))
             (member erization '(bare parenthesized none))
             (boolean? capitalized?)
             (not (and (equal? '(#\e #\r) segments)
                       (not (equal? 'none erization)))))
        (values segments tone erization capitalized?)
        (error (format "Bad ~a." name)))))

(struct polysyllable (syllables) #:transparent
  #:guard
  (λ (syllables name)
    (if (and (list? syllables)
             (not (empty? syllables))
             (andmap syllable? syllables))
        (values syllables)
        (error (format "Bad ~a." name)))))

(struct compound (polysyllables-and-strings) #:transparent
  #:guard
  (λ (polysyllables-and-strings name)
    (if (and (list? polysyllables-and-strings)
             (not (empty? polysyllables-and-strings))
             (andmap (disjoin polysyllable? string?)
                     polysyllables-and-strings))
        (values polysyllables-and-strings)
        (error (format "Bad ~a." name)))))

(define (capitalize str)
  (match (string->list str)
    [(cons first rest)
     (list->string (cons (char-upcase first) rest))]
    [empty
     ""]))

(define (syllable-segments/raw syllable)
  (cdr (some-value (pst-ref zhupin-pst (syllable-segments syllable)))))

(define (syllable-pin1yin1 syllable)
  (let ([segments (list->string (syllable-segments syllable))]
        [erization
         (case (syllable-erization syllable)
           [(bare) "r"]
           [(parenthesized) "(r)"]
           [(none) ""])]
        [tone
         (case (syllable-tone syllable)
           [(1) "1"]
           [(2) "2"]
           [(3) "3"]
           [(4) "4"]
           [(0) "5"])])
    (string-append (if (syllable-capitalized? syllable)
                       (capitalize segments)
                       segments)
                   erization
                   tone)))

(define (syllable-pinyin-core syllable)
  (match-let ([(list pre unmarked post) (syllable-segments/raw syllable)])
    (let* ([marked
            (vector-ref (case unmarked
                          [("a") #("a" "ā" "á" "ǎ" "à")]
                          [("e") #("e" "ē" "é" "ě" "è")]
                          [("ê") #("ê" "ê̄" "ế" "ê̌" "ề")]
                          [("i") #("i" "ī" "í" "ǐ" "ì")]
                          [("o") #("o" "ō" "ó" "ǒ" "ò")]
                          [("u") #("u" "ū" "ú" "ǔ" "ù")]
                          [("ü") #("ü" "ǖ" "ǘ" "ǚ" "ǜ")]
                          [("m") #("m" "m̄" "ḿ" "m̌" "m̀")]
                          [("n") #("n" "n̄" "ń" "ň" "ǹ")])
                        (syllable-tone syllable))]
           [core
            (string-append pre marked post)])
      (if (syllable-capitalized? syllable)
          (capitalize core)
          core))))

(define (syllable-zhuyin-core #:syllabic-m? syllabic-m?
                              #:syllabic-n? syllabic-n?
                              #:syllabic-ng? syllabic-ng?
                              #:explicit-empty-rhyme? explicit-empty-rhyme?
                              syllable)
  (match (car (some-value (pst-ref zhupin-pst (syllable-segments syllable))))
    [(and initial
          (or "ㄓ" "ㄔ" "ㄕ" "ㄖ" "ㄗ" "ㄘ" "ㄙ"))
     (if explicit-empty-rhyme?
         (string-append initial "ㄭ")
         initial)]
    ["ㄇ"
     (if syllabic-m? "ㆬ" "ㄇ")]
    ["ㄋ"
     (if syllabic-n? "ㄯ" "ㄋ")]
    ["ㄫ"
     (if syllabic-ng? "ㆭ" "ㄫ")]
    [core
     core]))

(define (syllable-zhuyin-tone-mark syllable)
  (case (syllable-tone syllable)
    [(0) "˙"]
    [(1) "ˉ"]
    [(2) "ˊ"]
    [(3) "ˇ"]
    [(4) "ˋ"]))
