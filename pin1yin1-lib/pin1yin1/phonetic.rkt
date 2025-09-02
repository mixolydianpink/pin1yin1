#lang racket/base

(provide (struct-out syllable)
         (struct-out polysyllable)
         (struct-out complex)

         syllable->pinyin-parts
         syllable->zhuyin-core
         syllable->zhuyin-tone-mark)

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

(struct complex (polysyllables-and-strings) #:transparent
  #:guard
  (λ (polysyllables-and-strings name)
    (if (and (list? polysyllables-and-strings)
             (not (empty? polysyllables-and-strings))
             (andmap (disjoin polysyllable? string?)
                     polysyllables-and-strings))
        (values polysyllables-and-strings)
        (error (format "Bad ~a." name)))))

(define (syllable->pinyin-parts syllable)
  (match-let ([(list pre unmarked post)
               (cdr (some-value (pst-ref zhupin-pst (syllable-segments syllable))))])
    (list pre
          (string
           (string-ref (case unmarked
                         [("a") "aāáǎà"]
                         [("e") "eēéěè"]
                         [("i") "iīíǐì"]
                         [("o") "oōóǒò"]
                         [("u") "uūúǔù"]
                         [("ü") "üǖǘǚǜ"])
                       (syllable-tone syllable)))
          post)))

(define (syllable->zhuyin-core syllable)
  (car (some-value (pst-ref zhupin-pst (syllable-segments syllable)))))

(define (syllable->zhuyin-tone-mark syllable)
  (case (syllable-tone syllable)
    [(0) "˙"]
    [(1) "ˉ"]
    [(2) "ˊ"]
    [(3) "ˇ"]
    [(4) "ˋ"]))
