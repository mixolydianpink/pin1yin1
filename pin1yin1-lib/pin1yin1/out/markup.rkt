#lang racket/base

(provide make-pin1yin1->pinyin/html-fragment
         make-pin1yin1->zhuyin/html-fragment

         (struct-out style)
         make-style
         (struct-out syllable-style)
         make-syllable-style

         html-fragment->string)

(require (only-in racket/function
                  curry
                  identity)
         racket/match
         (only-in racket/string
                  string-append*)
         (only-in xml
                  empty-tag-shorthand
                  html-empty-tags
                  xexpr->string)

         pin1yin1/markup
         pin1yin1/non-phonetic
         pin1yin1/non-phonetic/markup
         pin1yin1/non-phonetic/string
         pin1yin1/phonetic
         pin1yin1/phonetic/markup
         pin1yin1/phonetic/string
         pin1yin1/pin1yin1/markup
         pin1yin1/string)

(struct style (syllable polysyllable compound))

(define (make-style #:syllable [syllable 'plain]
                    #:component/word [polysyllable 'spliced]
                    #:compound/word [compound 'spliced])
  (style syllable polysyllable compound))

(struct syllable-style (format first-tone-class second-tone-class third-tone-class fourth-tone-class neutral-tone-class))

(define (make-syllable-style #:format [format 'plain]
                             #:first-tone [first-tone-class #f]
                             #:second-tone [second-tone-class #f]
                             #:third-tone [third-tone-class #f]
                             #:fourth-tone [fourth-tone-class #f]
                             #:neutral-tone [neutral-tone-class #f])
  (syllable-style format first-tone-class second-tone-class third-tone-class fourth-tone-class neutral-tone-class))

(define (html-fragment->string fragment)
  (parameterize ([empty-tag-shorthand (cons 'wbr html-empty-tags)])
    (string-append* (map xexpr->string
                         fragment))))

(define (make-pin1yin1->pinyin/html-fragment #:diacritic-e^? [diacritic-e^? #t]
                                             #:diacritic-m? [diacritic-m? #t]
                                             #:diacritic-n? [diacritic-n? #t]
                                             #:diacritic-ng? [diacritic-ng? #t]
                                             #:explicit-neutral-tone? [explicit-neutral-tone? #f]
                                             #:syllable-separator [syllable-separator 'none]
                                             #:capitals/numerals [capitals/numerals 'halfwidth]
                                             #:hyphen [hyphen 'hyphen]
                                             #:space [space 'halfwidth]
                                             #:underscore [underscore 'halfwidth]
                                             #:punctuation [punctuation 'zh-Latn]
                                             #:style [style (make-style)])
  (λ (pin1yin1)
    (pin1yin1->html-fragment #:compound->html-fragment
                             (compose (match (style-compound style)
                                        ['spliced identity]
                                        [(? string? class)
                                         (λ (fragment)
                                           `(,(html-fragment->html #:tag 'span
                                                                   #:class class
                                                                   fragment)))])
                                      (curry compound->html-fragment
                                             #:sep/html-fragment
                                             (case hyphen
                                               [(none) '()]
                                               [(hyphen) '("-")]
                                               [(hyphen/non-breaking) '(#x2011)]
                                               [(hyphen/soft) '(shy)]
                                               [(zero-width) '(#x200B)]
                                               [(zero-width/non-breaking) '(#x2060)] ; Word joiner
                                               [(halfwidth) '(" ")]
                                               [(halfwidth/non-breaking) '(#x00A0)]
                                               [else hyphen])
                                             #:polysyllable->html-fragment
                                             (compose (match (style-polysyllable style)
                                                        ['spliced identity]
                                                        [(? string? class)
                                                         (λ (fragment)
                                                           `(,(html-fragment->html #:tag 'span
                                                                                   #:class class
                                                                                   fragment)))])
                                                      (curry polysyllable->pinyin/html-fragment
                                                             #:sep/html-fragment
                                                             (case syllable-separator
                                                               [(none) '()]
                                                               [(hyphen/soft) '(shy)]
                                                               [(zero-width/non-breaking) '(#x2060)] ; Word joiner
                                                               [else syllable-separator])
                                                             #:syllable->pinyin/html-fragment
                                                             (compose list
                                                                      (if (equal? 'plain (style-syllable style))
                                                                          (curry syllable->pinyin
                                                                                 #:diacritic-e^? diacritic-e^?
                                                                                 #:diacritic-m? diacritic-m?
                                                                                 #:diacritic-n? diacritic-n?
                                                                                 #:diacritic-ng? diacritic-ng?
                                                                                 #:explicit-neutral-tone? explicit-neutral-tone?)
                                                                          (match-let ([(syllable-style format
                                                                                                       first-tone-class
                                                                                                       second-tone-class
                                                                                                       third-tone-class
                                                                                                       fourth-tone-class
                                                                                                       neutral-tone-class)
                                                                                       (style-syllable style)])
                                                                            (let ([syllable->
                                                                                   (case format
                                                                                     [(plain) syllable->pinyin/span]
                                                                                     [(structured) syllable->pinyin/span/structured])])
                                                                              (λ (#:suppress-leading-apostrophe? suppress-leading-apostrophe?
                                                                                  syllable)
                                                                                (let ([class (case (syllable-tone syllable)
                                                                                               [(0) neutral-tone-class]
                                                                                               [(1) first-tone-class]
                                                                                               [(2) second-tone-class]
                                                                                               [(3) third-tone-class]
                                                                                               [(4) fourth-tone-class])])
                                                                                  (syllable-> #:diacritic-e^? diacritic-e^?
                                                                                              #:diacritic-m? diacritic-m?
                                                                                              #:diacritic-n? diacritic-n?
                                                                                              #:diacritic-ng? diacritic-ng?
                                                                                              #:explicit-neutral-tone? explicit-neutral-tone?
                                                                                              #:class class
                                                                                              #:suppress-leading-apostrophe? suppress-leading-apostrophe?
                                                                                              syllable)))))))))
                                             #:string->html-fragment
                                             (compose string->html-fragment
                                                      (case capitals/numerals
                                                        [(halfwidth) identity]
                                                        [(fullwidth) string/fullwidth-capitals-and-numerals]))))
                             #:non-phonetic->html-fragment
                             (make-non-phonetic-> #:literal-> literal->html-fragment
                                                  #:whitespace->
                                                  (make-whitespace-> #:space
                                                                     (case space
                                                                       [(none) '()]
                                                                       [(zero-width) '(#x200B)]
                                                                       [(halfwidth) '(" ")]
                                                                       [(wbr) '((wbr))]
                                                                       [else space])
                                                                     #:underscore
                                                                     (case underscore
                                                                       [(none) '()]
                                                                       [(zero-width) '(#x200B)]
                                                                       [(halfwidth) '(" ")]
                                                                       [(wbr) '((wbr))]
                                                                       [else underscore])
                                                                     #:explicit-space '(" ")
                                                                     #:zero-width-space '(#x200B)
                                                                     #:fullwidth-space '(#x3000)
                                                                     #:tab '(Tab)
                                                                     #:newline '(NewLine))
                                                  #:punctuation->
                                                  (compose string->html-fragment
                                                           (->punctuation->string punctuation)))
                             pin1yin1)))

(define (make-pin1yin1->zhuyin/html-fragment #:syllabic-m? [syllabic-m? #f]
                                             #:syllabic-n? [syllabic-n? #f]
                                             #:syllabic-ng? [syllabic-ng? #f]
                                             #:explicit-empty-rhyme? [explicit-empty-rhyme? #f]
                                             #:explicit-first-tone? [explicit-first-tone? #f]
                                             #:prefix-neutral-tone? [prefix-neutral-tone? #f]
                                             #:syllable-separator [syllable-separator 'none]
                                             #:capitals/numerals [capitals/numerals 'fullwidth]
                                             #:hyphen [hyphen 'none]
                                             #:space [space 'none]
                                             #:underscore [underscore 'halfwidth]
                                             #:punctuation [punctuation 'zh-TW]
                                             #:style [style (make-style)])
  (λ (pin1yin1)
    (pin1yin1->html-fragment #:compound->html-fragment
                             (compose (match (style-compound style)
                                        ['spliced identity]
                                        [(? string? class)
                                         (λ (fragment)
                                           `(,(html-fragment->html #:tag 'span
                                                                   #:class class
                                                                   fragment)))])
                                      (curry compound->html-fragment
                                             #:sep/html-fragment
                                             (case hyphen
                                               [(none) '()]
                                               [(hyphen) '("-")]
                                               [(hyphen/non-breaking) '(#x2011)]
                                               [(hyphen/soft) '(shy)]
                                               [(zero-width) '(#x200B)]
                                               [(zero-width/non-breaking) '(#x2060)] ; Word joiner
                                               [(halfwidth) '(" ")]
                                               [(halfwidth/non-breaking) '(#x00A0)]
                                               [else hyphen])
                                             #:polysyllable->html-fragment
                                             (compose (match (style-polysyllable style)
                                                        ['spliced identity]
                                                        [(? string? class)
                                                         (λ (fragment)
                                                           `(,(html-fragment->html #:tag 'span
                                                                                   #:class class
                                                                                   fragment)))])
                                                      (curry polysyllable->zhuyin/html-fragment
                                                             #:sep/html-fragment
                                                             (case syllable-separator
                                                               [(none) '()]
                                                               [(hyphen/soft) '(shy)]
                                                               [(zero-width/non-breaking) '(#x2060)] ; Word joiner
                                                               [else syllable-separator])
                                                             #:syllable->zhuyin/html-fragment
                                                             (compose list
                                                                      (if (equal? 'plain (style-syllable style))
                                                                          (curry syllable->zhuyin
                                                                                 #:syllabic-m? syllabic-m?
                                                                                 #:syllabic-n? syllabic-n?
                                                                                 #:syllabic-ng? syllabic-ng?
                                                                                 #:explicit-empty-rhyme? explicit-empty-rhyme?
                                                                                 #:explicit-first-tone? explicit-first-tone?
                                                                                 #:prefix-neutral-tone? prefix-neutral-tone?)
                                                                          (match-let ([(syllable-style format
                                                                                                       first-tone-class
                                                                                                       second-tone-class
                                                                                                       third-tone-class
                                                                                                       fourth-tone-class
                                                                                                       neutral-tone-class)
                                                                                       (style-syllable style)])
                                                                            (let ([syllable->
                                                                                   (case format
                                                                                     [(plain) syllable->zhuyin/span]
                                                                                     [(structured) syllable->zhuyin/span/structured])])
                                                                              (λ (syllable)
                                                                                (let ([class (case (syllable-tone syllable)
                                                                                               [(0) neutral-tone-class]
                                                                                               [(1) first-tone-class]
                                                                                               [(2) second-tone-class]
                                                                                               [(3) third-tone-class]
                                                                                               [(4) fourth-tone-class])])
                                                                                  (syllable-> #:syllabic-m? syllabic-m?
                                                                                              #:syllabic-n? syllabic-n?
                                                                                              #:syllabic-ng? syllabic-ng?
                                                                                              #:explicit-empty-rhyme? explicit-empty-rhyme?
                                                                                              #:explicit-first-tone? explicit-first-tone?
                                                                                              #:prefix-neutral-tone? prefix-neutral-tone?
                                                                                              #:class class
                                                                                              syllable)))))))))
                                             #:string->html-fragment
                                             (compose string->html-fragment
                                                      (case capitals/numerals
                                                        [(halfwidth) identity]
                                                        [(fullwidth) string/fullwidth-capitals-and-numerals]))))
                             #:non-phonetic->html-fragment
                             (make-non-phonetic-> #:literal-> literal->html-fragment
                                                  #:whitespace->
                                                  (make-whitespace-> #:space
                                                                     (case space
                                                                       [(none) '()]
                                                                       [(zero-width) '(#x200B)]
                                                                       [(halfwidth) '(" ")]
                                                                       [(wbr) '((wbr))]
                                                                       [else space])
                                                                     #:underscore
                                                                     (case underscore
                                                                       [(none) '()]
                                                                       [(zero-width) '(#x200B)]
                                                                       [(halfwidth) '(" ")]
                                                                       [(wbr) '((wbr))]
                                                                       [else underscore])
                                                                     #:explicit-space '(" ")
                                                                     #:zero-width-space '(#x200B)
                                                                     #:fullwidth-space '(#x3000)
                                                                     #:tab '(Tab)
                                                                     #:newline '(NewLine))
                                                  #:punctuation->
                                                  (compose string->html-fragment
                                                           (->punctuation->string punctuation)))
                             pin1yin1)))
