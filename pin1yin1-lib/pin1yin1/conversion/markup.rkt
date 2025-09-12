#lang racket/base

(provide pin1yin1-string->pinyin/html-fragment
         pin1yin1-string->zhuyin/html-fragment
         pin1yin1-string->pinyin/html
         pin1yin1-string->zhuyin/html)

(require pin1yin1/conversion
         pin1yin1/markup
         pin1yin1/pin1yin1/markup)

(define (pin1yin1-string->pinyin/html-fragment #:interpret-v-as-u-umlaut? [interpret-v-as-u-umlaut? #t]
                                               #:interpret-e^-as-e-circumflex? [interpret-e^-as-e-circumflex? #t]
                                               #:implicit-neutral-tone? [implicit-neutral-tone? #f]
                                               #:explicit-neutral-tone? [explicit-neutral-tone? #f]
                                               #:space [space 'halfwidth]
                                               #:punctuation [punctuation 'zh-Latn]
                                               #:syllable-first-tone-class [syllable-first-tone-class #f]
                                               #:syllable-second-tone-class [syllable-second-tone-class #f]
                                               #:syllable-third-tone-class [syllable-third-tone-class #f]
                                               #:syllable-fourth-tone-class [syllable-fourth-tone-class #f]
                                               #:syllable-neutral-tone-class [syllable-neutral-tone-class #f]
                                               str)
  (let ([pin1yin1
         (pin1yin1-string->pin1yin1 #:interpret-v-as-u-umlaut? interpret-v-as-u-umlaut?
                                    #:interpret-e^-as-e-circumflex? interpret-e^-as-e-circumflex?
                                    #:implicit-neutral-tone? implicit-neutral-tone?
                                    str)])
    (and pin1yin1
         ((make-pin1yin1->pinyin/html-fragment #:explicit-neutral-tone? explicit-neutral-tone?
                                               #:space space
                                               #:punctuation punctuation
                                               #:syllable-first-tone-class syllable-first-tone-class
                                               #:syllable-second-tone-class syllable-second-tone-class
                                               #:syllable-third-tone-class syllable-third-tone-class
                                               #:syllable-fourth-tone-class syllable-fourth-tone-class
                                               #:syllable-neutral-tone-class syllable-neutral-tone-class)
          pin1yin1))))

(define (pin1yin1-string->zhuyin/html-fragment  #:interpret-v-as-u-umlaut? [interpret-v-as-u-umlaut? #t]
                                                #:interpret-e^-as-e-circumflex? [interpret-e^-as-e-circumflex? #t]
                                                #:implicit-neutral-tone? [implicit-neutral-tone? #f]
                                                #:syllabic-m? [syllabic-m? #f]
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
                                                #:syllable-neutral-tone-class [syllable-neutral-tone-class #f]
                                                str)
  (let ([pin1yin1
         (pin1yin1-string->pin1yin1 #:interpret-v-as-u-umlaut? interpret-v-as-u-umlaut?
                                    #:interpret-e^-as-e-circumflex? interpret-e^-as-e-circumflex?
                                    #:implicit-neutral-tone? implicit-neutral-tone?
                                    str)])
    (and pin1yin1
         ((make-pin1yin1->zhuyin/html-fragment #:syllabic-m? syllabic-m?
                                               #:syllabic-n? syllabic-n?
                                               #:syllabic-ng? syllabic-ng?
                                               #:explicit-empty-rhyme? explicit-empty-rhyme?
                                               #:explicit-first-tone? explicit-first-tone?
                                               #:prefix-neutral-tone? prefix-neutral-tone?
                                               #:space space
                                               #:punctuation punctuation
                                               #:syllable-first-tone-class syllable-first-tone-class
                                               #:syllable-second-tone-class syllable-second-tone-class
                                               #:syllable-third-tone-class syllable-third-tone-class
                                               #:syllable-fourth-tone-class syllable-fourth-tone-class
                                               #:syllable-neutral-tone-class syllable-neutral-tone-class)
          pin1yin1))))
                                               
(define (pin1yin1-string->pinyin/html #:tag [tag 'span]
                                      #:class [class #f]
                                      #:lang [lang "zh-Latn"]
                                      #:interpret-v-as-u-umlaut? [interpret-v-as-u-umlaut? #t]
                                      #:interpret-e^-as-e-circumflex? [interpret-e^-as-e-circumflex? #t]
                                      #:implicit-neutral-tone? [implicit-neutral-tone? #f]
                                      #:explicit-neutral-tone? [explicit-neutral-tone? #f]
                                      #:space [space 'halfwidth]
                                      #:punctuation [punctuation 'zh-Latn]
                                      #:syllable-first-tone-class [syllable-first-tone-class #f]
                                      #:syllable-second-tone-class [syllable-second-tone-class #f]
                                      #:syllable-third-tone-class [syllable-third-tone-class #f]
                                      #:syllable-fourth-tone-class [syllable-fourth-tone-class #f]
                                      #:syllable-neutral-tone-class [syllable-neutral-tone-class #f]
                                      str)
  (let ([fragment (pin1yin1-string->pinyin/html-fragment #:interpret-v-as-u-umlaut? interpret-v-as-u-umlaut?
                                                         #:interpret-e^-as-e-circumflex? interpret-e^-as-e-circumflex?
                                                         #:implicit-neutral-tone? implicit-neutral-tone?
                                                         #:explicit-neutral-tone? explicit-neutral-tone?
                                                         #:space space
                                                         #:punctuation punctuation
                                                         #:syllable-first-tone-class syllable-first-tone-class
                                                         #:syllable-second-tone-class syllable-second-tone-class
                                                         #:syllable-third-tone-class syllable-third-tone-class
                                                         #:syllable-fourth-tone-class syllable-fourth-tone-class
                                                         #:syllable-neutral-tone-class syllable-neutral-tone-class
                                                         str)])
    (and fragment
         (html-fragment->html #:tag tag
                              #:class class
                              #:lang lang
                              fragment))))

(define (pin1yin1-string->zhuyin/html #:tag [tag 'span]
                                      #:class [class #f]
                                      #:lang [lang "zh-TW"]
                                      #:interpret-v-as-u-umlaut? [interpret-v-as-u-umlaut? #t]
                                      #:interpret-e^-as-e-circumflex? [interpret-e^-as-e-circumflex? #t]
                                      #:implicit-neutral-tone? [implicit-neutral-tone? #f]
                                      #:syllabic-m? [syllabic-m? #f]
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
                                      #:syllable-neutral-tone-class [syllable-neutral-tone-class #f]
                                      str)
  (let ([fragment (pin1yin1-string->zhuyin/html-fragment #:interpret-v-as-u-umlaut? interpret-v-as-u-umlaut?
                                                         #:interpret-e^-as-e-circumflex? interpret-e^-as-e-circumflex?
                                                         #:implicit-neutral-tone? implicit-neutral-tone?
                                                         #:syllabic-m? syllabic-m?
                                                         #:syllabic-n? syllabic-n?
                                                         #:syllabic-ng? syllabic-ng?
                                                         #:explicit-empty-rhyme? explicit-empty-rhyme?
                                                         #:explicit-first-tone? explicit-first-tone?
                                                         #:prefix-neutral-tone? prefix-neutral-tone?
                                                         #:space space
                                                         #:punctuation punctuation
                                                         #:syllable-first-tone-class syllable-first-tone-class
                                                         #:syllable-second-tone-class syllable-second-tone-class
                                                         #:syllable-third-tone-class syllable-third-tone-class
                                                         #:syllable-fourth-tone-class syllable-fourth-tone-class
                                                         #:syllable-neutral-tone-class syllable-neutral-tone-class
                                                         str)])
    (and fragment
         (html-fragment->html #:tag tag
                              #:class class
                              #:lang lang
                              fragment))))
