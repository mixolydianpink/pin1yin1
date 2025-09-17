#lang racket/base

(provide syllable/p
         polysyllable/p
         compound/p)

(require (only-in racket/function
                  conjoin
                  const)
         (only-in racket/list
                  empty)
         racket/match

         pin1yin1/list
         pin1yin1/option
         pin1yin1/parse
         pin1yin1/phonetic
         pin1yin1/pst/parse
         pin1yin1/zhupin)

(define (syllable/p #:interpret-e^-as-e-circumflex? interpret-e^-as-e-circumflex?
                    #:interpret-v-as-u-umlaut? interpret-v-as-u-umlaut?
                    #:allow-capitalized? allow-capitalized?
                    #:implicit-neutral-tone? implicit-neutral-tone?)
  (let* ([first/p
          (or/p (if interpret-e^-as-e-circumflex?
                    (or/p (map/p (const #\Ê)
                                 (eq/p #\E #\^))
                          (map/p (const #\ê)
                                 (eq/p #\e #\^)))
                    never/p)
                (if/p char?))]
         [subsequent/p
          (or/p (if interpret-v-as-u-umlaut?
                    (or/p (map/p (const #\Ü)
                                 (eq/p #\V))
                          (map/p (const #\ü)
                                 (eq/p #\v)))
                    never/p)
                first/p)]
         [capitalized?+segments+erization/p
          (bind/p (λ (first)
                    (let ([capitalized? (char-upper-case? first)])
                      (bind/p (match-λ [(cons segments _)
                                        (if (equal? '(#\e #\r) segments)
                                            (pure/p (list capitalized? segments 'none))
                                            (map/p (λ (erization)
                                                     (list capitalized? segments erization))
                                                   (or/p (map/p (const 'bare)
                                                                (or/p (eq/p #\r)
                                                                      (eq/p #\' #\r)))
                                                         (map/p (const 'parenthesized)
                                                                (eq/p #\( #\r #\)))
                                                         (pure/p 'none))))])
                              (pst/p #:pst zhupin-pst
                                     #:prefix
                                     (list (if capitalized?
                                               (char-downcase first)
                                               first))
                                     #:key-fragment/p
                                     (map/p list
                                            subsequent/p)))))
                  first/p)]
         [tone/p
          (or/p (map/p (λ (char)
                         (case char
                           [(#\1) 1]
                           [(#\2) 2]
                           [(#\3) 3]
                           [(#\4) 4]
                           [(#\5) 0]))
                       (one-of/p '(#\1 #\2 #\3 #\4 #\5)))
                (pure/p #f))])
    (bind/p (match-λ [(list capitalized? segments erization)
                      (bind/p (λ (tone)
                                (if (or (and capitalized? (not allow-capitalized?))
                                        (and (not tone) (not implicit-neutral-tone?)))
                                    never/p
                                    (pure/p (syllable segments (or tone 0) erization capitalized?))))
                              tone/p)])
            capitalized?+segments+erization/p)))

(define (polysyllable/p #:implicit-neutral-tone? implicit-neutral-tone?
                        syllable/p)
  (let ([first/p
         (syllable/p #:allow-capitalized? #t
                     #:implicit-neutral-tone? #f)]
        [subsequent/p
         (map/p flatten1
                (seq/p (multi/p (syllable/p #:allow-capitalized? #f
                                            #:implicit-neutral-tone? #f))
                       (or/p (syllable/p #:allow-capitalized? #f
                                         #:implicit-neutral-tone? implicit-neutral-tone?)
                             (pure/p empty))))])
    (or/p (map/p polysyllable
                 (then/p first/p
                         subsequent/p))
          ; Allow single implicitly neutral tone syllable if it is *not* one uppercase letter.
          (if implicit-neutral-tone?
              (map-partial/p (λ (syllable)
                               (if (and (syllable-capitalized? syllable)
                                        (= 1 (length (syllable-segments syllable))))
                                   (none)
                                   (some (polysyllable (list syllable)))))
                             (syllable/p #:allow-capitalized? #t
                                         #:implicit-neutral-tone? #t))
              never/p))))

(define capitals (string->list "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
(define numerals (string->list "0123456789"))

(define (compound/p polysyllable/p)
  (define (capital? char) (if (member char capitals char=?) #t #f))
  (define (numeral? char) (if (member char numerals char=?) #t #f))
  (let ([polysyllable/capitals/numerals/p
         (or/p polysyllable/p
               (map/p list->string
                      (or/p (multi+/p (if/p (conjoin char? capital?)))
                            (multi+/p (if/p (conjoin char? numeral?))))))])
    (map/p compound
           (sep-by/p #:sep/p (eq/p #\-)
                     polysyllable/capitals/numerals/p))))
