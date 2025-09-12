#lang racket/base

(provide pin1yin1->pinyin/html-fragment
         make-pin1yin1->pinyin/html-fragment
         pin1yin1->zhuyin/html-fragment
         make-pin1yin1->zhuyin/html-fragment)

(require (only-in racket/function
                  curry)

         pin1yin1/markup
         pin1yin1/non-phonetic
         pin1yin1/non-phonetic/markup
         pin1yin1/non-phonetic/string
         pin1yin1/phonetic/markup
         pin1yin1/pin1yin1)

(define (pin1yin1->html-fragment #:compound->html-fragment compound->html-fragment
                                 #:non-phonetic->html-fragment non-phonetic->html-fragment
                                 pin1yin1)
  (pin1yin1-append-map #:compound->list compound->html-fragment
                       #:non-phonetic->list non-phonetic->html-fragment
                       pin1yin1))

(define (pin1yin1->pinyin/html-fragment #:explicit-neutral-tone? explicit-neutral-tone?
                                        #:syllable-first-tone-class syllable-first-tone-class
                                        #:syllable-second-tone-class syllable-second-tone-class
                                        #:syllable-third-tone-class syllable-third-tone-class
                                        #:syllable-fourth-tone-class syllable-fourth-tone-class
                                        #:syllable-neutral-tone-class syllable-neutral-tone-class
                                        #:non-phonetic->html-fragment non-phonetic->html-fragment
                                        pin1yin1)
  (pin1yin1->html-fragment #:compound->html-fragment
                           (curry compound->pinyin/html-fragment
                                  #:explicit-neutral-tone? explicit-neutral-tone?
                                  #:syllable-first-tone-class syllable-first-tone-class
                                  #:syllable-second-tone-class syllable-second-tone-class
                                  #:syllable-third-tone-class syllable-third-tone-class
                                  #:syllable-fourth-tone-class syllable-fourth-tone-class
                                  #:syllable-neutral-tone-class syllable-neutral-tone-class)
                           #:non-phonetic->html-fragment non-phonetic->html-fragment
                           pin1yin1))

(define (make-pin1yin1->pinyin/html-fragment #:explicit-neutral-tone? [explicit-neutral-tone? #f]
                                             #:space [space 'halfwidth]
                                             #:punctuation [punctuation 'zh-Latn]
                                             #:syllable-first-tone-class [syllable-first-tone-class #f]
                                             #:syllable-second-tone-class [syllable-second-tone-class #f]
                                             #:syllable-third-tone-class [syllable-third-tone-class #f]
                                             #:syllable-fourth-tone-class [syllable-fourth-tone-class #f]
                                             #:syllable-neutral-tone-class [syllable-neutral-tone-class #f])
  (λ (pin1yin1)
    (pin1yin1->pinyin/html-fragment #:explicit-neutral-tone? explicit-neutral-tone?
                                    #:syllable-first-tone-class syllable-first-tone-class
                                    #:syllable-second-tone-class syllable-second-tone-class
                                    #:syllable-third-tone-class syllable-third-tone-class
                                    #:syllable-fourth-tone-class syllable-fourth-tone-class
                                    #:syllable-neutral-tone-class syllable-neutral-tone-class
                                    #:non-phonetic->html-fragment
                                    (curry non-phonetic->
                                           #:literal-> literal->html-fragment
                                           #:whitespace->
                                           (make-whitespace-> #:space
                                                              (case space
                                                                [(none) '()]
                                                                [(zero-width) '(#x200B)]
                                                                [(halfwidth) '(" ")]
                                                                [(fullwidth) '(#x3000)])
                                                              #:explicit-space '(" ")
                                                              #:zero-width-space '(#x200B)
                                                              #:fullwidth-space '(#x3000)
                                                              #:tab '(Tab)
                                                              #:newline '(NewLine))
                                           #:punctuation->
                                           (compose string->html-fragment
                                                    (->punctuation->string punctuation)))
                                    pin1yin1)))

(define (pin1yin1->zhuyin/html-fragment #:syllabic-m? syllabic-m?
                                        #:syllabic-n? syllabic-n?
                                        #:syllabic-ng? syllabic-ng?
                                        #:explicit-empty-rhyme? explicit-empty-rhyme?
                                        #:explicit-first-tone? explicit-first-tone?
                                        #:prefix-neutral-tone? prefix-neutral-tone?
                                        #:syllable-first-tone-class syllable-first-tone-class
                                        #:syllable-second-tone-class syllable-second-tone-class
                                        #:syllable-third-tone-class syllable-third-tone-class
                                        #:syllable-fourth-tone-class syllable-fourth-tone-class
                                        #:syllable-neutral-tone-class syllable-neutral-tone-class
                                        #:non-phonetic->html-fragment non-phonetic->html-fragment
                                        pin1yin1)
  (pin1yin1->html-fragment #:compound->html-fragment
                           (curry compound->zhuyin/html-fragment
                                  #:syllabic-m? syllabic-m?
                                  #:syllabic-n? syllabic-n?
                                  #:syllabic-ng? syllabic-ng?
                                  #:explicit-empty-rhyme? explicit-empty-rhyme?
                                  #:explicit-first-tone? explicit-first-tone?
                                  #:prefix-neutral-tone? prefix-neutral-tone?
                                  #:syllable-first-tone-class syllable-first-tone-class
                                  #:syllable-second-tone-class syllable-second-tone-class
                                  #:syllable-third-tone-class syllable-third-tone-class
                                  #:syllable-fourth-tone-class syllable-fourth-tone-class
                                  #:syllable-neutral-tone-class syllable-neutral-tone-class)
                           #:non-phonetic->html-fragment non-phonetic->html-fragment
                           pin1yin1))

(define (make-pin1yin1->zhuyin/html-fragment #:syllabic-m? [syllabic-m? #f]
                                             #:syllabic-n? [syllabic-n? #f]
                                             #:syllabic-ng? [syllabic-ng? #f]
                                             #:explicit-empty-rhyme? [explicit-empty-rhyme? #f]
                                             #:explicit-first-tone? [explicit-first-tone? #f]
                                             #:prefix-neutral-tone? [prefix-neutral-tone? #f]
                                             #:space [space 'none]
                                             #:punctuation [punctuation 'zh-TW]
                                             #:syllable-first-tone-class [syllable-first-tone-class #f]
                                             #:syllable-second-tone-class [syllable-second-tone-class #f]
                                             #:syllable-third-tone-class [syllable-third-tone-class #f]
                                             #:syllable-fourth-tone-class [syllable-fourth-tone-class #f]
                                             #:syllable-neutral-tone-class [syllable-neutral-tone-class #f])
  (λ (pin1yin1)
    (pin1yin1->zhuyin/html-fragment #:syllabic-m? syllabic-m?
                                    #:syllabic-n? syllabic-n?
                                    #:syllabic-ng? syllabic-ng?
                                    #:explicit-empty-rhyme? explicit-empty-rhyme?
                                    #:explicit-first-tone? explicit-first-tone?
                                    #:prefix-neutral-tone? prefix-neutral-tone?
                                    #:syllable-first-tone-class syllable-first-tone-class
                                    #:syllable-second-tone-class syllable-second-tone-class
                                    #:syllable-third-tone-class syllable-third-tone-class
                                    #:syllable-fourth-tone-class syllable-fourth-tone-class
                                    #:syllable-neutral-tone-class syllable-neutral-tone-class
                                    #:non-phonetic->html-fragment
                                    (curry non-phonetic->
                                           #:literal-> literal->html-fragment
                                           #:whitespace->
                                           (make-whitespace-> #:space
                                                              (case space
                                                                [(none) '()]
                                                                [(zero-width) '(#x200B)]
                                                                [(halfwidth) '(" ")]
                                                                [(fullwidth) '(#x3000)])
                                                              #:explicit-space '(" ")
                                                              #:zero-width-space '(#x200B)
                                                              #:fullwidth-space '(#x3000)
                                                              #:tab '(Tab)
                                                              #:newline '(NewLine))
                                           #:punctuation->
                                           (compose string->html-fragment
                                                    (->punctuation->string punctuation)))
                                    pin1yin1)))
