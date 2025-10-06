#lang racket/base

(provide make-pin1yin1->pinyin/html-fragment
         make-pin1yin1->zhuyin/html-fragment

         html-fragment->string)

(require (only-in racket/function
                  curry
                  identity)
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
         pin1yin1/pin1yin1/markup
         pin1yin1/string)

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
                                             #:syllable-first-tone-class [syllable-first-tone-class #f]
                                             #:syllable-second-tone-class [syllable-second-tone-class #f]
                                             #:syllable-third-tone-class [syllable-third-tone-class #f]
                                             #:syllable-fourth-tone-class [syllable-fourth-tone-class #f]
                                             #:syllable-neutral-tone-class [syllable-neutral-tone-class #f])
  (λ (pin1yin1)
    (pin1yin1->html-fragment #:compound->html-fragment
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
                                    (curry polysyllable->pinyin/html-fragment
                                           #:sep/html-fragment
                                           (case syllable-separator
                                             [(none) '()]
                                             [(hyphen/soft) '(shy)]
                                             [(zero-width/non-breaking) '(#x2060)] ; Word joiner
                                             [else syllable-separator])
                                           #:syllable->pinyin/html-fragment
                                           (compose list
                                                    (λ (#:suppress-leading-apostrophe? suppress-leading-apostrophe?
                                                        syllable)
                                                      (let ([class (case (syllable-tone syllable)
                                                                     [(0) syllable-neutral-tone-class]
                                                                     [(1) syllable-first-tone-class]
                                                                     [(2) syllable-second-tone-class]
                                                                     [(3) syllable-third-tone-class]
                                                                     [(4) syllable-fourth-tone-class])])
                                                        (syllable->pinyin/span #:diacritic-e^? diacritic-e^?
                                                                               #:diacritic-m? diacritic-m?
                                                                               #:diacritic-n? diacritic-n?
                                                                               #:diacritic-ng? diacritic-ng?
                                                                               #:explicit-neutral-tone? explicit-neutral-tone?
                                                                               #:class class
                                                                               #:suppress-leading-apostrophe? suppress-leading-apostrophe?
                                                                               syllable)))))
                                    #:string->html-fragment
                                    (compose string->html-fragment
                                             (case capitals/numerals
                                               [(halfwidth) identity]
                                               [(fullwidth) string/fullwidth-capitals-and-numerals])))
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
                                             #:syllable-first-tone-class [syllable-first-tone-class #f]
                                             #:syllable-second-tone-class [syllable-second-tone-class #f]
                                             #:syllable-third-tone-class [syllable-third-tone-class #f]
                                             #:syllable-fourth-tone-class [syllable-fourth-tone-class #f]
                                             #:syllable-neutral-tone-class [syllable-neutral-tone-class #f])
  (λ (pin1yin1)
    (pin1yin1->html-fragment #:compound->html-fragment
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
                                    (curry polysyllable->zhuyin/html-fragment
                                           #:sep/html-fragment
                                           (case syllable-separator
                                             [(none) '()]
                                             [(hyphen/soft) '(shy)]
                                             [(zero-width/non-breaking) '(#x2060)] ; Word joiner
                                             [else syllable-separator])
                                           #:syllable->zhuyin/html-fragment
                                           (compose list
                                                    (λ (syllable)
                                                      (let ([class (case (syllable-tone syllable)
                                                                     [(0) syllable-neutral-tone-class]
                                                                     [(1) syllable-first-tone-class]
                                                                     [(2) syllable-second-tone-class]
                                                                     [(3) syllable-third-tone-class]
                                                                     [(4) syllable-fourth-tone-class])])
                                                        (syllable->zhuyin/span #:syllabic-m? syllabic-m?
                                                                               #:syllabic-n? syllabic-n?
                                                                               #:syllabic-ng? syllabic-ng?
                                                                               #:explicit-empty-rhyme? explicit-empty-rhyme?
                                                                               #:explicit-first-tone? explicit-first-tone?
                                                                               #:prefix-neutral-tone? prefix-neutral-tone?
                                                                               #:class class
                                                                               syllable)))))
                                    #:string->html-fragment
                                    (compose string->html-fragment
                                             (case capitals/numerals
                                               [(halfwidth) identity]
                                               [(fullwidth) string/fullwidth-capitals-and-numerals])))
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
