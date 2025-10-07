#lang racket/base

(provide pin1yin1-string->pinyin/html-fragment
         pin1yin1-string->zhuyin/html-fragment)

(require pin1yin1/in
         pin1yin1/out/markup)

(define (pin1yin1-string->pinyin/html-fragment #:interpret-e^-as-e-circumflex? [interpret-e^-as-e-circumflex? #t]
                                               #:interpret-v-as-u-umlaut? [interpret-v-as-u-umlaut? #t]
                                               #:implicit-neutral-tone? [implicit-neutral-tone? #f]
                                               #:diacritic-e^? [diacritic-e^? #t]
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
                                               #:style [style (make-style)]
                                               string)
  (let ([pin1yin1
         (pin1yin1-string->pin1yin1 #:interpret-e^-as-e-circumflex? interpret-e^-as-e-circumflex?
                                    #:interpret-v-as-u-umlaut? interpret-v-as-u-umlaut?
                                    #:implicit-neutral-tone? implicit-neutral-tone?
                                    string)])
    (and pin1yin1
         ((make-pin1yin1->pinyin/html-fragment #:diacritic-e^? diacritic-e^?
                                               #:diacritic-m? diacritic-m?
                                               #:diacritic-n? diacritic-n?
                                               #:diacritic-ng? diacritic-ng?
                                               #:explicit-neutral-tone? explicit-neutral-tone?
                                               #:syllable-separator syllable-separator
                                               #:capitals/numerals capitals/numerals
                                               #:hyphen hyphen
                                               #:space space
                                               #:underscore underscore
                                               #:punctuation punctuation
                                               #:style style)
          pin1yin1))))

(define (pin1yin1-string->zhuyin/html-fragment #:interpret-e^-as-e-circumflex? [interpret-e^-as-e-circumflex? #t]
                                               #:interpret-v-as-u-umlaut? [interpret-v-as-u-umlaut? #t]
                                               #:implicit-neutral-tone? [implicit-neutral-tone? #f]
                                               #:syllabic-m? [syllabic-m? #f]
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
                                               #:style [style (make-style)]
                                               string)
  (let ([pin1yin1
         (pin1yin1-string->pin1yin1 #:interpret-e^-as-e-circumflex? interpret-e^-as-e-circumflex?
                                    #:interpret-v-as-u-umlaut? interpret-v-as-u-umlaut?
                                    #:implicit-neutral-tone? implicit-neutral-tone?
                                    string)])
    (and pin1yin1
         ((make-pin1yin1->zhuyin/html-fragment #:syllabic-m? syllabic-m?
                                               #:syllabic-n? syllabic-n?
                                               #:syllabic-ng? syllabic-ng?
                                               #:explicit-empty-rhyme? explicit-empty-rhyme?
                                               #:explicit-first-tone? explicit-first-tone?
                                               #:prefix-neutral-tone? prefix-neutral-tone?
                                               #:syllable-separator syllable-separator
                                               #:capitals/numerals capitals/numerals
                                               #:hyphen hyphen
                                               #:space space
                                               #:underscore underscore
                                               #:punctuation punctuation
                                               #:style style)
          pin1yin1))))
