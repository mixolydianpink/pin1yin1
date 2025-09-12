#lang racket/base

(provide syllable->pinyin
         syllable->zhuyin
         polysyllable->pinyin
         polysyllable->zhuyin
         compound->string
         compound->pinyin
         compound->zhuyin)

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
  (match-let ([(list pre marked post) (syllable-pinyin-parts syllable)])
    (let ([pinyin (string-append pre
                                 marked
                                 post
                                 (case (syllable-erization syllable)
                                   [(bare) "r"]
                                   [(parenthesized) "(r)"]
                                   [(none) ""]))])
      (string-append (if (and (not suppress-leading-apostrophe?)
                              (member (car (syllable-segments syllable)) '(#\a #\e #\o) char=?))
                         "'"
                         "")
                     (if (and explicit-neutral-tone? (= 0 (syllable-tone syllable)))
                         "·"
                         "")
                     pinyin))))

(define (syllable->zhuyin #:explicit-first-tone? explicit-first-tone?
                          #:prefix-neutral-tone? prefix-neutral-tone?
                          #:explicit-empty-rhyme? explicit-empty-rhyme?
                          #:syllabic-m? syllabic-m?
                          #:syllabic-n? syllabic-n?
                          #:syllabic-ng? syllabic-ng?
                          syllable)
  (let ([core
         (syllable-zhuyin-core #:explicit-empty-rhyme? explicit-empty-rhyme?
                               #:syllabic-m? syllabic-m?
                               #:syllabic-n? syllabic-n?
                               #:syllabic-ng? syllabic-ng?
                               syllable)]
        [tone (syllable-tone syllable)])
    (string-append (if (and prefix-neutral-tone? (= 0 tone))
                       (syllable-zhuyin-tone-mark syllable)
                       "")
                   core
                   (if (or (and prefix-neutral-tone? (= 0 tone))
                           (and (not explicit-first-tone?) (= 1 tone)))
                       ""
                       (syllable-zhuyin-tone-mark syllable))
                   (case (syllable-erization syllable)
                     [(bare) "ㄦ"]
                     [(parenthesized) "（ㄦ）"]
                     [(none) ""]))))

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

(define (compound->string #:sep sep
                          #:polysyllable->string polysyllable->string
                          #:string->string string->string
                          compound)
  (string-append* (add-between (for/list ([polysyllable-or-string (compound-polysyllables-and-strings compound)])
                                 (match polysyllable-or-string
                                   [(? polysyllable? polysyllable) (polysyllable->string polysyllable)]
                                   [(? string? str) (string->string str)]))
                               sep)))

(define (compound->pinyin #:explicit-neutral-tone? explicit-neutral-tone?
                          compound)
  (compound->string #:sep "-"
                    #:polysyllable->string
                    (curry polysyllable->pinyin
                           #:syllable->pinyin
                           (curry syllable->pinyin
                                  #:explicit-neutral-tone? explicit-neutral-tone?))
                    #:string->string identity
                    compound))

(define (compound->zhuyin #:explicit-first-tone? explicit-first-tone?
                          #:prefix-neutral-tone? prefix-neutral-tone?
                          #:explicit-empty-rhyme? explicit-empty-rhyme?
                          #:syllabic-m? syllabic-m?
                          #:syllabic-n? syllabic-n?
                          #:syllabic-ng? syllabic-ng?
                          compound)
  (compound->string #:sep ""
                    #:polysyllable->string
                    (curry polysyllable->zhuyin
                           #:syllable->zhuyin
                           (curry syllable->zhuyin
                                  #:explicit-first-tone? explicit-first-tone?
                                  #:prefix-neutral-tone? prefix-neutral-tone?
                                  #:explicit-empty-rhyme? explicit-empty-rhyme?
                                  #:syllabic-m? syllabic-m?
                                  #:syllabic-n? syllabic-n?
                                  #:syllabic-ng? syllabic-ng?))
                    #:string->string identity
                    compound))
