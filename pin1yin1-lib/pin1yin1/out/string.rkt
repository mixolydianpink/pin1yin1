#lang racket/base

(provide make-pin1yin1->pinyin
         make-pin1yin1->zhuyin)

(require (only-in racket/function
                  curry
                  identity)

         pin1yin1/non-phonetic
         pin1yin1/non-phonetic/string
         pin1yin1/phonetic/string
         pin1yin1/pin1yin1/string)

(define (make-pin1yin1->pinyin #:diacritic-e^? [diacritic-e^? #t]
                               #:diacritic-m? [diacritic-m? #t]
                               #:diacritic-n? [diacritic-n? #t]
                               #:diacritic-ng? [diacritic-ng? #t]
                               #:explicit-neutral-tone? [explicit-neutral-tone? #f]
                               #:space [space 'halfwidth]
                               #:punctuation [punctuation 'zh-Latn])
  (λ (pin1yin1)
    (pin1yin1->string #:compound->string
                      (curry compound->string
                             #:sep "-"
                             #:polysyllable->string
                             (curry polysyllable->pinyin
                                    #:syllable->pinyin
                                    (curry syllable->pinyin
                                           #:diacritic-e^? diacritic-e^?
                                           #:diacritic-m? diacritic-m?
                                           #:diacritic-n? diacritic-n?
                                           #:diacritic-ng? diacritic-ng?
                                           #:explicit-neutral-tone? explicit-neutral-tone?))
                             #:string->string identity)
                      #:non-phonetic->string
                      (curry non-phonetic->
                             #:literal-> literal-content
                             #:whitespace->
                             (make-whitespace-> #:space
                                                (case space
                                                  [(none) ""]
                                                  [(zero-width) "\u200B"]
                                                  [(halfwidth) " "]
                                                  [(fullwidth) "\u3000"])
                                                #:explicit-space " "
                                                #:zero-width-space "\u200B"
                                                #:fullwidth-space "\u3000"
                                                #:tab "\t"
                                                #:newline "\n")
                             #:punctuation-> (->punctuation->string punctuation))
                      pin1yin1)))

(define (make-pin1yin1->zhuyin #:syllabic-m? [syllabic-m? #f]
                               #:syllabic-n? [syllabic-n? #f]
                               #:syllabic-ng? [syllabic-ng? #f]
                               #:explicit-empty-rhyme? [explicit-empty-rhyme? #f]
                               #:explicit-first-tone? [explicit-first-tone? #f]
                               #:prefix-neutral-tone? [prefix-neutral-tone? #f]
                               #:space [space 'none]
                               #:punctuation [punctuation 'zh-TW])
  (λ (pin1yin1)
    (pin1yin1->string #:compound->string
                      (curry compound->string #:sep ""
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
                             #:string->string identity)
                      #:non-phonetic->string
                      (curry non-phonetic->
                             #:literal-> literal-content
                             #:whitespace->
                             (make-whitespace-> #:space
                                                (case space
                                                  [(none) ""]
                                                  [(zero-width) "\u200B"]
                                                  [(halfwidth) " "]
                                                  [(fullwidth) "\u3000"])
                                                #:explicit-space " "
                                                #:zero-width-space "\u200B"
                                                #:fullwidth-space "\u3000"
                                                #:tab "\t"
                                                #:newline "\n")
                             #:punctuation-> (->punctuation->string punctuation))
                      pin1yin1)))
