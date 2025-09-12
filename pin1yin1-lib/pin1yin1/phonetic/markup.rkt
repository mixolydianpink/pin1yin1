#lang racket/base

(provide syllable->pinyin/span
         syllable->zhuyin/span
         polysyllable->pinyin/html-fragment
         polysyllable->zhuyin/html-fragment
         compound->html-fragment
         compound->pinyin/html-fragment
         compound->zhuyin/html-fragment)

(require (only-in racket/function
                  curry)
         (only-in racket/list
                  [add-between list:add-between]
                  append-map)
         racket/match

         pin1yin1/list
         pin1yin1/markup
         pin1yin1/option
         pin1yin1/phonetic
         pin1yin1/phonetic/string)

(define (add-between lst sep-opt)
  (match sep-opt
    [(some sep)
     (list:add-between lst sep)]
    [(none)
     lst]))

(define (syllable->pinyin/span #:explicit-neutral-tone? explicit-neutral-tone?
                               #:first-tone-class first-tone-class
                               #:second-tone-class second-tone-class
                               #:third-tone-class third-tone-class
                               #:fourth-tone-class fourth-tone-class
                               #:neutral-tone-class neutral-tone-class
                               #:suppress-leading-apostrophe? suppress-leading-apostrophe?
                               syllable)
  (let ([pinyin (syllable->pinyin #:explicit-neutral-tone? explicit-neutral-tone?
                                  #:suppress-leading-apostrophe? suppress-leading-apostrophe?
                                  syllable)]
        [class (case (syllable-tone syllable)
                 [(0) neutral-tone-class]
                 [(1) first-tone-class]
                 [(2) second-tone-class]
                 [(3) third-tone-class]
                 [(4) fourth-tone-class])])
    (html-fragment->html #:tag 'span
                         #:class class
                         (string->html-fragment pinyin))))

(define (syllable->zhuyin/span #:explicit-first-tone? explicit-first-tone?
                               #:prefix-neutral-tone? prefix-neutral-tone?
                               #:syllabic-m? syllabic-m?
                               #:syllabic-n? syllabic-n?
                               #:syllabic-ng? syllabic-ng?
                               #:explicit-empty-rhyme? explicit-empty-rhyme?
                               #:first-tone-class first-tone-class
                               #:second-tone-class second-tone-class
                               #:third-tone-class third-tone-class
                               #:fourth-tone-class fourth-tone-class
                               #:neutral-tone-class neutral-tone-class
                               syllable)
  (let* ([core
          (syllable-zhuyin-core #:syllabic-m? syllabic-m?
                                #:syllabic-n? syllabic-n?
                                #:syllabic-ng? syllabic-ng?
                                #:explicit-empty-rhyme? explicit-empty-rhyme?
                                syllable)]
         [tone (syllable-tone syllable)]
         [class (case tone
                  [(0) neutral-tone-class]
                  [(1) first-tone-class]
                  [(2) second-tone-class]
                  [(3) third-tone-class]
                  [(4) fourth-tone-class])])
    (html-fragment->html #:tag 'span
                         #:class class
                         `(,(case tone
                              [(0)
                               (if prefix-neutral-tone?
                                   `(span (span ,(syllable-zhuyin-tone-mark syllable))
                                          ,core)
                                   `(span ,core
                                          (span ,(syllable-zhuyin-tone-mark syllable))))]
                              [(1)
                               (if explicit-first-tone?
                                   `(span ,core
                                          (span ,(syllable-zhuyin-tone-mark syllable)))
                                   core)]
                              [(2 3 4)
                               `(span ,core
                                      (span ,(syllable-zhuyin-tone-mark syllable)))])
                           ,@(case (syllable-erization syllable)
                               [(bare) '("ㄦ")]
                               [(parenthesized) '("（ㄦ）")]
                               [(none) '()])))))

(define (polysyllable->pinyin/html-fragment #:syllable->pinyin/html-fragment syllable->pinyin/html-fragment
                                            polysyllable)
  (match-let ([(cons first rest) (polysyllable-syllables polysyllable)])
    (flatten1 (cons (syllable->pinyin/html-fragment #:suppress-leading-apostrophe? #t
                                                    first)
                    (for/list ([syllable rest])
                      (syllable->pinyin/html-fragment #:suppress-leading-apostrophe? #f
                                                      syllable))))))

(define (polysyllable->zhuyin/html-fragment #:syllable->zhuyin/html-fragment syllable->zhuyin/html-fragment
                                            polysyllable)
  (append-map syllable->zhuyin/html-fragment (polysyllable-syllables polysyllable)))

(define (compound->html-fragment #:sep sep
                                 #:polysyllable->html-fragment polysyllable->html-fragment
                                 #:string->html-fragment string->html-fragment
                                 compound)
  (flatten1 (add-between (for/list ([polysyllable-or-string (compound-polysyllables-and-strings compound)])
                           (match polysyllable-or-string
                             [(? polysyllable? polysyllable)
                              (polysyllable->html-fragment polysyllable)]
                             [(? string? string)
                              (string->html-fragment string)]))
                         (option-map list sep))))

(define (compound->pinyin/html-fragment #:explicit-neutral-tone? explicit-neutral-tone?
                                        #:syllable-first-tone-class syllable-first-tone-class
                                        #:syllable-second-tone-class syllable-second-tone-class
                                        #:syllable-third-tone-class syllable-third-tone-class
                                        #:syllable-fourth-tone-class syllable-fourth-tone-class
                                        #:syllable-neutral-tone-class syllable-neutral-tone-class
                                        compound)
  (compound->html-fragment #:sep (some "-")
                           #:polysyllable->html-fragment
                           (curry polysyllable->pinyin/html-fragment
                                  #:syllable->pinyin/html-fragment
                                  (compose list
                                           (curry syllable->pinyin/span
                                                  #:explicit-neutral-tone? explicit-neutral-tone?
                                                  #:first-tone-class syllable-first-tone-class
                                                  #:second-tone-class syllable-second-tone-class
                                                  #:third-tone-class syllable-third-tone-class
                                                  #:fourth-tone-class syllable-fourth-tone-class
                                                  #:neutral-tone-class syllable-neutral-tone-class)))
                           #:string->html-fragment string->html-fragment
                           compound))

(define (compound->zhuyin/html-fragment #:explicit-first-tone? explicit-first-tone?
                                        #:prefix-neutral-tone? prefix-neutral-tone?
                                        #:syllabic-m? syllabic-m?
                                        #:syllabic-n? syllabic-n?
                                        #:syllabic-ng? syllabic-ng?
                                        #:explicit-empty-rhyme? explicit-empty-rhyme?
                                        #:syllable-first-tone-class syllable-first-tone-class
                                        #:syllable-second-tone-class syllable-second-tone-class
                                        #:syllable-third-tone-class syllable-third-tone-class
                                        #:syllable-fourth-tone-class syllable-fourth-tone-class
                                        #:syllable-neutral-tone-class syllable-neutral-tone-class
                                        compound)
  (compound->html-fragment #:sep (none)
                           #:polysyllable->html-fragment
                           (curry polysyllable->zhuyin/html-fragment
                                  #:syllable->zhuyin/html-fragment
                                  (compose list
                                           (curry syllable->zhuyin/span
                                                  #:explicit-first-tone? explicit-first-tone?
                                                  #:prefix-neutral-tone? prefix-neutral-tone?
                                                  #:syllabic-m? syllabic-m?
                                                  #:syllabic-n? syllabic-n?
                                                  #:syllabic-ng? syllabic-ng?
                                                  #:explicit-empty-rhyme? explicit-empty-rhyme?
                                                  #:first-tone-class syllable-first-tone-class
                                                  #:second-tone-class syllable-second-tone-class
                                                  #:third-tone-class syllable-third-tone-class
                                                  #:fourth-tone-class syllable-fourth-tone-class
                                                  #:neutral-tone-class syllable-neutral-tone-class)))
                           #:string->html-fragment string->html-fragment
                           compound))
