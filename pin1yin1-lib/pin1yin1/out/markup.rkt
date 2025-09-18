#lang racket/base

(provide make-pin1yin1->pinyin/html-fragment
         make-pin1yin1->zhuyin/html-fragment

         html-fragment->string)

(require (only-in racket/function
                  curry)
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
         pin1yin1/option
         pin1yin1/phonetic/markup
         pin1yin1/pin1yin1/markup)

(define (html-fragment->string fragment)
  (parameterize ([empty-tag-shorthand (cons 'wbr html-empty-tags)])
    (string-append* (map xexpr->string
                         fragment))))

(define (make-pin1yin1->pinyin/html-fragment #:diacritic-e^? [diacritic-e^? #t]
                                             #:diacritic-m? [diacritic-m? #t]
                                             #:diacritic-n? [diacritic-n? #t]
                                             #:diacritic-ng? [diacritic-ng? #t]
                                             #:explicit-neutral-tone? [explicit-neutral-tone? #f]
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
                                    #:sep (some "-")
                                    #:polysyllable->html-fragment
                                    (curry polysyllable->pinyin/html-fragment
                                           #:syllable->pinyin/html-fragment
                                           (compose list
                                                    (curry syllable->pinyin/span
                                                           #:diacritic-e^? diacritic-e^?
                                                           #:diacritic-m? diacritic-m?
                                                           #:diacritic-n? diacritic-n?
                                                           #:diacritic-ng? diacritic-ng?
                                                           #:explicit-neutral-tone? explicit-neutral-tone?
                                                           #:first-tone-class syllable-first-tone-class
                                                           #:second-tone-class syllable-second-tone-class
                                                           #:third-tone-class syllable-third-tone-class
                                                           #:fourth-tone-class syllable-fourth-tone-class
                                                           #:neutral-tone-class syllable-neutral-tone-class)))
                                    #:string->html-fragment string->html-fragment)
                             #:non-phonetic->html-fragment
                             (make-non-phonetic-> #:literal-> literal->html-fragment
                                                  #:whitespace->
                                                  (make-whitespace-> #:space
                                                                     (case space
                                                                       [(none) '()]
                                                                       [(zero-width) '(#x200B)]
                                                                       [(halfwidth) '(" ")]
                                                                       [(fullwidth) '(#x3000)]
                                                                       [(wbr) '((wbr))])
                                                                     #:underscore
                                                                     (case underscore
                                                                       [(none) '()]
                                                                       [(zero-width) '(#x200B)]
                                                                       [(halfwidth) '(" ")]
                                                                       [(fullwidth) '(#x3000)]
                                                                       [(wbr) '((wbr))])
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
                                    #:sep (none)
                                    #:polysyllable->html-fragment
                                    (curry polysyllable->zhuyin/html-fragment
                                           #:syllable->zhuyin/html-fragment
                                           (compose list
                                                    (curry syllable->zhuyin/span
                                                           #:syllabic-m? syllabic-m?
                                                           #:syllabic-n? syllabic-n?
                                                           #:syllabic-ng? syllabic-ng?
                                                           #:explicit-empty-rhyme? explicit-empty-rhyme?
                                                           #:explicit-first-tone? explicit-first-tone?
                                                           #:prefix-neutral-tone? prefix-neutral-tone?
                                                           #:first-tone-class syllable-first-tone-class
                                                           #:second-tone-class syllable-second-tone-class
                                                           #:third-tone-class syllable-third-tone-class
                                                           #:fourth-tone-class syllable-fourth-tone-class
                                                           #:neutral-tone-class syllable-neutral-tone-class)))
                                    #:string->html-fragment string->html-fragment)
                             #:non-phonetic->html-fragment
                             (make-non-phonetic-> #:literal-> literal->html-fragment
                                                  #:whitespace->
                                                  (make-whitespace-> #:space
                                                                     (case space
                                                                       [(none) '()]
                                                                       [(zero-width) '(#x200B)]
                                                                       [(halfwidth) '(" ")]
                                                                       [(fullwidth) '(#x3000)]
                                                                       [(wbr) '((wbr))])
                                                                     #:underscore
                                                                     (case underscore
                                                                       [(none) '()]
                                                                       [(zero-width) '(#x200B)]
                                                                       [(halfwidth) '(" ")]
                                                                       [(fullwidth) '(#x3000)]
                                                                       [(wbr) '((wbr))])
                                                                     #:explicit-space '(" ")
                                                                     #:zero-width-space '(#x200B)
                                                                     #:fullwidth-space '(#x3000)
                                                                     #:tab '(Tab)
                                                                     #:newline '(NewLine))
                                                  #:punctuation->
                                                  (compose string->html-fragment
                                                           (->punctuation->string punctuation)))
                             pin1yin1)))
