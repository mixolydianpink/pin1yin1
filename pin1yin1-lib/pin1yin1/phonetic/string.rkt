#lang racket/base

(provide syllable->pinyin
         syllable->zhuyin
         polysyllable->pinyin
         polysyllable->zhuyin
         complex->string
         complex->pinyin
         complex->zhuyin)

(require (only-in racket/function
                  curry
                  identity)
         (only-in racket/list
                  add-between)
         racket/match
         (only-in racket/string
                  string-append*)

         pin1yin1/phonetic)

(define (syllable->pinyin #:explicit-neutral-tone? explicit-neutral-tone?
                          #:suppress-leading-apostrophe? suppress-leading-apostrophe?
                          syllable)
  (define (capitalize str)
    (match (string->list str)
      [(cons first rest)
       (list->string (cons (char-upcase first) rest))]
      [empty
       ""]))
  (match-let ([(list pre marked post) (syllable->pinyin-parts syllable)])
    (let ([pinyin (string-append pre
                                 marked
                                 post
                                 (if (syllable-erized? syllable) "r" ""))])
      (string-append (if (and (not suppress-leading-apostrophe?)
                              (member (car (syllable-segments syllable)) '(#\a #\e #\o) char=?))
                         "'"
                         "")
                     (if (and explicit-neutral-tone? (= 0 (syllable-tone syllable)))
                         "·"
                         "")
                     (if (syllable-capitalized? syllable)
                         (capitalize pinyin)
                         pinyin)))))

(define (syllable->zhuyin #:explicit-first-tone? explicit-first-tone?
                          #:prefix-neutral-tone? prefix-neutral-tone?
                          syllable)
  (let ([core (syllable->zhuyin-core syllable)]
        [tone (syllable-tone syllable)])
    (string-append (if (and prefix-neutral-tone? (= 0 tone))
                       (syllable->zhuyin-tone-mark syllable)
                       "")
                   core
                   (if (or (and prefix-neutral-tone? (= 0 tone))
                           (and (not explicit-first-tone?) (= 1 tone)))
                       ""
                       (syllable->zhuyin-tone-mark syllable))
                   (if (syllable-erized? syllable)
                       "ㄦ"
                       ""))))

(define (polysyllable->pinyin #:syllable->pinyin syllable->pinyin
                              polysyllable)
  (match-let ([(cons first rest) (polysyllable-syllables polysyllable)])
    (string-append* (syllable->pinyin #:suppress-leading-apostrophe? #t
                                      first)
                    (for/list ([syllable rest])
                      (syllable->pinyin #:suppress-leading-apostrophe? #f
                                        syllable)))))

(define (polysyllable->zhuyin #:syllable->zhuyin syllable->zhuyin
                              polysyllable)
  (string-append* (for/list ([syllable (polysyllable-syllables polysyllable)])
                    (syllable->zhuyin syllable))))

(define (complex->string #:sep sep
                         #:polysyllable->string polysyllable->string
                         #:string->string string->string
                         complex)
  (string-append* (add-between (for/list ([polysyllable-or-string (complex-polysyllables-and-strings complex)])
                                 (match polysyllable-or-string
                                   [(? polysyllable? polysyllable) (polysyllable->string polysyllable)]
                                   [(? string? str) (string->string str)]))
                               sep)))

(define (complex->pinyin #:explicit-neutral-tone? explicit-neutral-tone?
                         complex)
  (complex->string #:sep "‑"
                   #:polysyllable->string
                   (curry polysyllable->pinyin
                          #:syllable->pinyin
                          (curry syllable->pinyin
                                 #:explicit-neutral-tone? explicit-neutral-tone?))
                   #:string->string identity
                   complex))

(define (complex->zhuyin #:explicit-first-tone? explicit-first-tone?
                         #:prefix-neutral-tone? prefix-neutral-tone?
                         complex)
  (complex->string #:sep ""
                   #:polysyllable->string
                   (curry polysyllable->zhuyin
                          #:syllable->zhuyin
                          (curry syllable->zhuyin
                                 #:explicit-first-tone? explicit-first-tone?
                                 #:prefix-neutral-tone? prefix-neutral-tone?))
                   #:string->string identity
                   complex))
