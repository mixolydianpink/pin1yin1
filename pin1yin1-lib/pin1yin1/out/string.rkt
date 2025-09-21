#lang racket/base

(provide make-pin1yin1->pinyin
         make-pin1yin1->zhuyin)

(require (only-in racket/function
                  curry
                  identity)

         pin1yin1/non-phonetic
         pin1yin1/non-phonetic/string
         pin1yin1/phonetic/string
         pin1yin1/pin1yin1/string
         pin1yin1/string)

(define (make-pin1yin1->pinyin #:diacritic-e^? [diacritic-e^? #t]
                               #:diacritic-m? [diacritic-m? #t]
                               #:diacritic-n? [diacritic-n? #t]
                               #:diacritic-ng? [diacritic-ng? #t]
                               #:explicit-neutral-tone? [explicit-neutral-tone? #f]
                               #:syllable-separator [syllable-separator 'none]
                               #:capitals/numerals [capitals/numerals 'halfwidth]
                               #:hyphen [hyphen 'hyphen]
                               #:space [space 'halfwidth]
                               #:underscore [underscore 'halfwidth]
                               #:punctuation [punctuation 'zh-Latn])
  (λ (pin1yin1)
    (pin1yin1->string #:compound->string
                      (curry compound->string
                             #:sep
                             (case hyphen
                               [(none) ""]
                               [(hyphen) "-"]
                               [(hyphen/non-breaking) "\u2011"]
                               [(zero-width) "\u200B"]
                               [(zero-width/non-breaking) "\u2060"] ; Word joiner
                               [(halfwidth) " "]
                               [(halfwidth/non-breaking) "\u00A0"]
                               [else hyphen])
                             #:polysyllable->string
                             (curry polysyllable->pinyin
                                    #:sep
                                    (case syllable-separator
                                      [(none) ""]
                                      [(zero-width/non-breaking) "\u2060"] ; Word joiner
                                      [else syllable-separator])
                                    #:syllable->pinyin
                                    (curry syllable->pinyin
                                           #:diacritic-e^? diacritic-e^?
                                           #:diacritic-m? diacritic-m?
                                           #:diacritic-n? diacritic-n?
                                           #:diacritic-ng? diacritic-ng?
                                           #:explicit-neutral-tone? explicit-neutral-tone?))
                             #:string->string (case capitals/numerals
                                                [(halfwidth) identity]
                                                [(fullwidth) string/fullwidth-capitals-and-numerals]))
                      #:non-phonetic->string
                      (make-non-phonetic-> #:literal-> literal-content
                                           #:whitespace->
                                           (make-whitespace-> #:space
                                                              (case space
                                                                [(none) ""]
                                                                [(zero-width) "\u200B"]
                                                                [(halfwidth) " "]
                                                                [else space])
                                                              #:underscore
                                                              (case underscore
                                                                [(none) ""]
                                                                [(zero-width) "\u200B"]
                                                                [(halfwidth) " "]
                                                                [else underscore])
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
                               #:syllable-separator [syllable-separator 'none]
                               #:capitals/numerals [capitals/numerals 'fullwidth]
                               #:hyphen [hyphen 'none]
                               #:space [space 'none]
                               #:underscore [underscore 'halfwidth]
                               #:punctuation [punctuation 'zh-TW])
  (λ (pin1yin1)
    (pin1yin1->string #:compound->string
                      (curry compound->string
                             #:sep
                             (case hyphen
                               [(none) ""]
                               [(hyphen) "-"]
                               [(hyphen/non-breaking) "\u2011"]
                               [(zero-width) "\u200B"]
                               [(zero-width/non-breaking) "\u2060"] ; Word joiner
                               [(halfwidth) " "]
                               [(halfwidth/non-breaking) "\u00A0"]
                               [else hyphen])
                             #:polysyllable->string
                             (curry polysyllable->zhuyin
                                    #:sep
                                    (case syllable-separator
                                      [(none) ""]
                                      [(zero-width/non-breaking) "\u2060"] ; Word joiner
                                      [else syllable-separator])
                                    #:syllable->zhuyin
                                    (curry syllable->zhuyin
                                           #:syllabic-m? syllabic-m?
                                           #:syllabic-n? syllabic-n?
                                           #:syllabic-ng? syllabic-ng?
                                           #:explicit-empty-rhyme? explicit-empty-rhyme?
                                           #:explicit-first-tone? explicit-first-tone?
                                           #:prefix-neutral-tone? prefix-neutral-tone?))
                             #:string->string (case capitals/numerals
                                                [(halfwidth) identity]
                                                [(fullwidth) string/fullwidth-capitals-and-numerals]))
                      #:non-phonetic->string
                      (make-non-phonetic-> #:literal-> literal-content
                                           #:whitespace->
                                           (make-whitespace-> #:space
                                                              (case space
                                                                [(none) ""]
                                                                [(zero-width) "\u200B"]
                                                                [(halfwidth) " "]
                                                                [else space])
                                                              #:underscore
                                                              (case underscore
                                                                [(none) ""]
                                                                [(zero-width) "\u200B"]
                                                                [(halfwidth) " "]
                                                                [else underscore])
                                                              #:explicit-space " "
                                                              #:zero-width-space "\u200B"
                                                              #:fullwidth-space "\u3000"
                                                              #:tab "\t"
                                                              #:newline "\n")
                                           #:punctuation-> (->punctuation->string punctuation))
                      pin1yin1)))
