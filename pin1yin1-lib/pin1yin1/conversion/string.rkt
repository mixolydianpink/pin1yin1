#lang racket/base

(provide pin1yin1-string->pinyin
         pin1yin1-string->zhuyin)

(require pin1yin1/conversion
         pin1yin1/pin1yin1/string)

(define (pin1yin1-string->pinyin #:interpret-e^-as-e-circumflex? [interpret-e^-as-e-circumflex? #t]
                                 #:interpret-v-as-u-umlaut? [interpret-v-as-u-umlaut? #t]
                                 #:implicit-neutral-tone? [implicit-neutral-tone? #f]
                                 #:diacritic-e^? [diacritic-e^? #t]
                                 #:diacritic-m? [diacritic-m? #t]
                                 #:diacritic-n? [diacritic-n? #t]
                                 #:diacritic-ng? [diacritic-ng? #t]
                                 #:explicit-neutral-tone? [explicit-neutral-tone? #f]
                                 #:space [space 'halfwidth]
                                 #:punctuation [punctuation 'zh-Latn]
                                 str)
  (let ([pin1yin1
         (pin1yin1-string->pin1yin1 #:interpret-e^-as-e-circumflex? interpret-e^-as-e-circumflex?
                                    #:interpret-v-as-u-umlaut? interpret-v-as-u-umlaut?
                                    #:implicit-neutral-tone? implicit-neutral-tone?
                                    str)])
    (and pin1yin1
         ((make-pin1yin1->pinyin #:diacritic-e^? diacritic-e^?
                                 #:diacritic-m? diacritic-m?
                                 #:diacritic-n? diacritic-n?
                                 #:diacritic-ng? diacritic-ng?
                                 #:explicit-neutral-tone? explicit-neutral-tone?
                                 #:space space
                                 #:punctuation punctuation)
          pin1yin1))))

(define (pin1yin1-string->zhuyin #:interpret-e^-as-e-circumflex? [interpret-e^-as-e-circumflex? #t]
                                 #:interpret-v-as-u-umlaut? [interpret-v-as-u-umlaut? #t]
                                 #:implicit-neutral-tone? [implicit-neutral-tone? #f]
                                 #:syllabic-m? [syllabic-m? #f]
                                 #:syllabic-n? [syllabic-n? #f]
                                 #:syllabic-ng? [syllabic-ng? #f]
                                 #:explicit-empty-rhyme? [explicit-empty-rhyme? #f]
                                 #:explicit-first-tone? [explicit-first-tone? #f]
                                 #:prefix-neutral-tone? [prefix-neutral-tone? #f]
                                 #:space [space 'none]
                                 #:punctuation [punctuation 'zh-TW]
                                 str)
  (let ([pin1yin1
         (pin1yin1-string->pin1yin1 #:interpret-e^-as-e-circumflex? interpret-e^-as-e-circumflex?
                                    #:interpret-v-as-u-umlaut? interpret-v-as-u-umlaut?
                                    #:implicit-neutral-tone? implicit-neutral-tone?
                                    str)])
    (and pin1yin1
         ((make-pin1yin1->zhuyin #:syllabic-m? syllabic-m?
                                 #:syllabic-n? syllabic-n?
                                 #:syllabic-ng? syllabic-ng?
                                 #:explicit-empty-rhyme? explicit-empty-rhyme?
                                 #:explicit-first-tone? explicit-first-tone?
                                 #:prefix-neutral-tone? prefix-neutral-tone?
                                 #:space space
                                 #:punctuation punctuation)
          pin1yin1))))
