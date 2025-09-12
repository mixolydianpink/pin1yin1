#lang racket/base

(provide pin1yin1-string->pinyin
         pin1yin1-string->zhuyin)

(require (only-in racket/function
                  curry)

         pin1yin1/conversion
         pin1yin1/non-phonetic
         pin1yin1/non-phonetic/parse
         pin1yin1/non-phonetic/string
         pin1yin1/parse
         pin1yin1/phonetic/string
         pin1yin1/pin1yin1/parse
         pin1yin1/pin1yin1/string)

(define (pin1yin1-string->pinyin #:implicit-neutral-tone? [implicit-neutral-tone? #f]
                                 #:interpret-v-as-u-umlaut? [interpret-v-as-u-umlaut? #t]
                                 #:interpret-e^-as-e-circumflex? [interpret-e^-as-e-circumflex? #t]
                                 #:explicit-neutral-tone? [explicit-neutral-tone? #f]
                                 #:space [space 'halfwidth]
                                 #:punctuation [punctuation 'zh-Latn]
                                 str)
  (let ([pin1yin1
         (pin1yin1-string->pin1yin1 #:implicit-neutral-tone? implicit-neutral-tone?
                                    #:interpret-v-as-u-umlaut? interpret-v-as-u-umlaut?
                                    #:interpret-e^-as-e-circumflex? interpret-e^-as-e-circumflex?
                                    str)])
    (and pin1yin1
         ((make-pin1yin1->pinyin #:explicit-neutral-tone? explicit-neutral-tone?
                                 #:space space
                                 #:punctuation punctuation)
          pin1yin1))))

(define (pin1yin1-string->zhuyin #:implicit-neutral-tone? [implicit-neutral-tone? #f]
                                 #:interpret-v-as-u-umlaut? [interpret-v-as-u-umlaut? #t]
                                 #:interpret-e^-as-e-circumflex? [interpret-e^-as-e-circumflex? #t]
                                 #:explicit-first-tone? [explicit-first-tone? #f]
                                 #:prefix-neutral-tone? [prefix-neutral-tone? #f]
                                 #:explicit-empty-rhyme? [explicit-empty-rhyme? #f]
                                 #:syllabic-m? [syllabic-m? #f]
                                 #:syllabic-n? [syllabic-n? #f]
                                 #:syllabic-ng? [syllabic-ng? #f]
                                 #:space [space 'none]
                                 #:punctuation [punctuation 'zh-TW]
                                 str)
  (let ([pin1yin1
         (pin1yin1-string->pin1yin1 #:implicit-neutral-tone? implicit-neutral-tone?
                                    #:interpret-v-as-u-umlaut? interpret-v-as-u-umlaut?
                                    #:interpret-e^-as-e-circumflex? interpret-e^-as-e-circumflex?
                                    str)])
    (and pin1yin1
         ((make-pin1yin1->zhuyin #:explicit-first-tone? explicit-first-tone?
                                 #:prefix-neutral-tone? prefix-neutral-tone?
                                 #:explicit-empty-rhyme? explicit-empty-rhyme?
                                 #:syllabic-m? syllabic-m?
                                 #:syllabic-n? syllabic-n?
                                 #:syllabic-ng? syllabic-ng?
                                 #:space space
                                 #:punctuation punctuation)
          pin1yin1))))
