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
                  string-append*
                  string-prefix?)

         pin1yin1/phonetic)

(define (syllable->pinyin #:diacritic-e^? diacritic-e^?
                          #:diacritic-m? diacritic-m?
                          #:diacritic-n? diacritic-n?
                          #:diacritic-ng? diacritic-ng?
                          #:explicit-neutral-tone? explicit-neutral-tone?
                          #:suppress-leading-apostrophe? suppress-leading-apostrophe?
                          syllable)
  (match-let ([(list pre unmarked post) (syllable-segments/raw syllable)])
    (let* ([neutral-tone? (= 0 (syllable-tone syllable))]
           [numbered?
            (and (not neutral-tone?)
                 (case unmarked
                   [("ê") (not diacritic-e^?)]
                   [("m") (not diacritic-m?)]
                   [("n") (or (and (not (string-prefix? post "g"))
                                   (not diacritic-n?))
                              (and (string-prefix? post "g")
                                   (not diacritic-ng?)))]
                   [else #f]))]
           [pinyin (if numbered?
                       (syllable-pin1yin1 syllable)
                       (string-append (if (and explicit-neutral-tone? neutral-tone?)
                                          "·"
                                          "")
                                      (syllable-pinyin-core syllable)
                                      (case (syllable-erization syllable)
                                        [(bare) "r"]
                                        [(parenthesized) "(r)"]
                                        [(none) ""])))])
      (string-append (if (and (not suppress-leading-apostrophe?)
                              (equal? "" pre))
                         "'"
                         "")
                     pinyin))))

(define (syllable->zhuyin #:syllabic-m? syllabic-m?
                          #:syllabic-n? syllabic-n?
                          #:syllabic-ng? syllabic-ng?
                          #:explicit-empty-rhyme? explicit-empty-rhyme?
                          #:explicit-first-tone? explicit-first-tone?
                          #:prefix-neutral-tone? prefix-neutral-tone?
                          syllable)
  (let ([core
         (syllable-zhuyin-core #:syllabic-m? syllabic-m?
                               #:syllabic-n? syllabic-n?
                               #:syllabic-ng? syllabic-ng?
                               #:explicit-empty-rhyme? explicit-empty-rhyme?
                               syllable)]
        [tone (syllable-tone syllable)])
    (string-append (case tone
                     [(0)
                      (if prefix-neutral-tone?
                          (string-append (syllable-zhuyin-tone-mark syllable)
                                         core)
                          (string-append core
                                         (syllable-zhuyin-tone-mark syllable)))]
                     [(1)
                      (if explicit-first-tone?
                          (string-append core
                                         (syllable-zhuyin-tone-mark syllable))
                          core)]
                     [(2 3 4)
                      (string-append core
                                     (syllable-zhuyin-tone-mark syllable))])
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

(define (compound->pinyin #:diacritic-e^? diacritic-e^?
                          #:diacritic-m? diacritic-m?
                          #:diacritic-n? diacritic-n?
                          #:diacritic-ng? diacritic-ng?
                          #:explicit-neutral-tone? explicit-neutral-tone?
                          compound)
  (compound->string #:sep "-"
                    #:polysyllable->string
                    (curry polysyllable->pinyin
                           #:syllable->pinyin
                           (curry syllable->pinyin
                                  #:diacritic-e^? diacritic-e^?
                                  #:diacritic-m? diacritic-m?
                                  #:diacritic-n? diacritic-n?
                                  #:diacritic-ng? diacritic-ng?
                                  #:explicit-neutral-tone? explicit-neutral-tone?))
                    #:string->string identity
                    compound))

(define (compound->zhuyin #:syllabic-m? syllabic-m?
                          #:syllabic-n? syllabic-n?
                          #:syllabic-ng? syllabic-ng?
                          #:explicit-empty-rhyme? explicit-empty-rhyme?
                          #:explicit-first-tone? explicit-first-tone?
                          #:prefix-neutral-tone? prefix-neutral-tone?
                          compound)
  (compound->string #:sep ""
                    #:polysyllable->string
                    (curry polysyllable->zhuyin
                           #:syllable->zhuyin
                           (curry syllable->zhuyin
                                  #:syllabic-m? syllabic-m?
                                  #:syllabic-n? syllabic-n?
                                  #:syllabic-ng? syllabic-ng?
                                  #:explicit-empty-rhyme? explicit-empty-rhyme?
                                  #:explicit-first-tone? explicit-first-tone?
                                  #:prefix-neutral-tone? prefix-neutral-tone?))
                    #:string->string identity
                    compound))
