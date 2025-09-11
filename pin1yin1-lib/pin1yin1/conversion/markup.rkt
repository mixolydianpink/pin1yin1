#lang racket/base

(provide pin1yin1-string->pinyin/html-fragment
         pin1yin1-string->zhuyin/html-fragment
         pin1yin1-string->pinyin/html
         pin1yin1-string->zhuyin/html)

(require (only-in racket/function
                  curry)

         pin1yin1/markup
         pin1yin1/non-phonetic
         pin1yin1/non-phonetic/markup
         pin1yin1/non-phonetic/parse
         pin1yin1/non-phonetic/string
         pin1yin1/parse
         pin1yin1/pin1yin1/markup
         pin1yin1/pin1yin1/parse)

(define (pin1yin1-string->pinyin/html-fragment #:implicit-neutral-tone? [implicit-neutral-tone? #f]
                                               #:interpret-v-as-u-umlaut? [interpret-v-as-u-umlaut? #t]
                                               #:interpret-e^-as-e-circumflex? [interpret-e^-as-e-circumflex? #t]
                                               #:explicit-neutral-tone? [explicit-neutral-tone? #f]
                                               #:punctuation [punctuation 'zh-Latn]
                                               #:syllable-first-tone-class [syllable-first-tone-class #f]
                                               #:syllable-second-tone-class [syllable-second-tone-class #f]
                                               #:syllable-third-tone-class [syllable-third-tone-class #f]
                                               #:syllable-fourth-tone-class [syllable-fourth-tone-class #f]
                                               #:syllable-neutral-tone-class [syllable-neutral-tone-class #f]
                                               str)
  (parse-string! (or/p (left/p (apply/p (pure/p (curry pin1yin1->pinyin/html-fragment
                                                       #:explicit-neutral-tone? explicit-neutral-tone?
                                                       #:syllable-first-tone-class syllable-first-tone-class
                                                       #:syllable-second-tone-class syllable-second-tone-class
                                                       #:syllable-third-tone-class syllable-third-tone-class
                                                       #:syllable-fourth-tone-class syllable-fourth-tone-class
                                                       #:syllable-neutral-tone-class syllable-neutral-tone-class
                                                       #:non-phonetic->html-fragment
                                                       (curry non-phonetic->
                                                              #:literal-> literal->html-fragment
                                                              #:punctuation->
                                                              (compose string->html-fragment
                                                                       (->punctuation->string punctuation)))))
                                        (make-pin1yin1/p #:implicit-neutral-tone?
                                                         implicit-neutral-tone?
                                                         #:interpret-v-as-u-umlaut?
                                                         interpret-v-as-u-umlaut?
                                                         #:interpret-e^-as-e-circumflex?
                                                         interpret-e^-as-e-circumflex?
                                                         #:non-phonetic/p non-phonetic/p))
                               (eos/p null))
                       (pure/p #f))
                 str))

(define (pin1yin1-string->zhuyin/html-fragment  #:implicit-neutral-tone? [implicit-neutral-tone? #f]
                                                #:interpret-v-as-u-umlaut? [interpret-v-as-u-umlaut? #t]
                                                #:interpret-e^-as-e-circumflex? [interpret-e^-as-e-circumflex? #t]
                                                #:explicit-first-tone? [explicit-first-tone? #f]
                                                #:prefix-neutral-tone? [prefix-neutral-tone? #f]
                                                #:explicit-empty-rhyme? [explicit-empty-rhyme? #f]
                                                #:syllabic-m? [syllabic-m? #f]
                                                #:syllabic-n? [syllabic-n? #f]
                                                #:syllabic-ng? [syllabic-ng? #f]
                                                #:punctuation [punctuation 'zh-TW]
                                                #:syllable-first-tone-class [syllable-first-tone-class #f]
                                                #:syllable-second-tone-class [syllable-second-tone-class #f]
                                                #:syllable-third-tone-class [syllable-third-tone-class #f]
                                                #:syllable-fourth-tone-class [syllable-fourth-tone-class #f]
                                                #:syllable-neutral-tone-class [syllable-neutral-tone-class #f]
                                                str)
  (parse-string! (or/p (left/p (apply/p (pure/p (curry pin1yin1->zhuyin/html-fragment
                                                       #:explicit-first-tone? explicit-first-tone?
                                                       #:prefix-neutral-tone? prefix-neutral-tone?
                                                       #:explicit-empty-rhyme? explicit-empty-rhyme?
                                                       #:syllabic-m? syllabic-m?
                                                       #:syllabic-n? syllabic-n?
                                                       #:syllabic-ng? syllabic-ng?
                                                       #:syllable-first-tone-class syllable-first-tone-class
                                                       #:syllable-second-tone-class syllable-second-tone-class
                                                       #:syllable-third-tone-class syllable-third-tone-class
                                                       #:syllable-fourth-tone-class syllable-fourth-tone-class
                                                       #:syllable-neutral-tone-class syllable-neutral-tone-class
                                                       #:non-phonetic->html-fragment
                                                       (curry non-phonetic->
                                                              #:literal-> literal->html-fragment
                                                              #:punctuation->
                                                              (compose string->html-fragment
                                                                       (->punctuation->string punctuation)))))
                                        (make-pin1yin1/p #:implicit-neutral-tone?
                                                         implicit-neutral-tone?
                                                         #:interpret-v-as-u-umlaut?
                                                         interpret-v-as-u-umlaut?
                                                         #:interpret-e^-as-e-circumflex?
                                                         interpret-e^-as-e-circumflex?
                                                         #:non-phonetic/p non-phonetic/p))
                               (eos/p null))
                       (pure/p #f))
                 str))
                                               

(define (pin1yin1-string->pinyin/html #:tag [tag 'span]
                                      #:class [class #f]
                                      #:lang [lang #f #|"zh-Latn"|#]
                                      #:implicit-neutral-tone? [implicit-neutral-tone? #f]
                                      #:interpret-v-as-u-umlaut? [interpret-v-as-u-umlaut? #t]
                                      #:interpret-e^-as-e-circumflex? [interpret-e^-as-e-circumflex? #t]
                                      #:explicit-neutral-tone? [explicit-neutral-tone? #f]
                                      #:punctuation [punctuation 'zh-Latn]
                                      #:syllable-first-tone-class [syllable-first-tone-class #f]
                                      #:syllable-second-tone-class [syllable-second-tone-class #f]
                                      #:syllable-third-tone-class [syllable-third-tone-class #f]
                                      #:syllable-fourth-tone-class [syllable-fourth-tone-class #f]
                                      #:syllable-neutral-tone-class [syllable-neutral-tone-class #f]
                                      str)
  (let ([fragment (pin1yin1-string->pinyin/html-fragment #:implicit-neutral-tone? implicit-neutral-tone?
                                                         #:explicit-neutral-tone? explicit-neutral-tone?
                                                         #:punctuation punctuation
                                                         #:syllable-first-tone-class syllable-first-tone-class
                                                         #:syllable-second-tone-class syllable-second-tone-class
                                                         #:syllable-third-tone-class syllable-third-tone-class
                                                         #:syllable-fourth-tone-class syllable-fourth-tone-class
                                                         #:syllable-neutral-tone-class syllable-neutral-tone-class
                                                         str)])
    (and fragment
         `(,tag (,@(attr 'class class)
                 ,@(attr 'lang lang))
                ,@fragment))))

(define (pin1yin1-string->zhuyin/html #:tag [tag 'span]
                                      #:class [class #f]
                                      #:lang [lang #f #|"zh-TW"|#]
                                      #:implicit-neutral-tone? [implicit-neutral-tone? #f]
                                      #:interpret-v-as-u-umlaut? [interpret-v-as-u-umlaut? #t]
                                      #:interpret-e^-as-e-circumflex? [interpret-e^-as-e-circumflex? #t]
                                      #:explicit-first-tone? [explicit-first-tone? #f]
                                      #:prefix-neutral-tone? [prefix-neutral-tone? #f]
                                      #:explicit-empty-rhyme? [explicit-empty-rhyme? #f]
                                      #:syllabic-m? [syllabic-m? #f]
                                      #:syllabic-n? [syllabic-n? #f]
                                      #:syllabic-ng? [syllabic-ng? #f]
                                      #:punctuation [punctuation 'zh-TW]
                                      #:syllable-first-tone-class [syllable-first-tone-class #f]
                                      #:syllable-second-tone-class [syllable-second-tone-class #f]
                                      #:syllable-third-tone-class [syllable-third-tone-class #f]
                                      #:syllable-fourth-tone-class [syllable-fourth-tone-class #f]
                                      #:syllable-neutral-tone-class [syllable-neutral-tone-class #f]
                                      str)
  (let ([fragment (pin1yin1-string->zhuyin/html-fragment #:implicit-neutral-tone? implicit-neutral-tone?
                                                         #:explicit-first-tone? explicit-first-tone?
                                                         #:prefix-neutral-tone? prefix-neutral-tone?
                                                         #:explicit-empty-rhyme? explicit-empty-rhyme?
                                                         #:syllabic-m? syllabic-m?
                                                         #:syllabic-n? syllabic-n?
                                                         #:syllabic-ng? syllabic-ng?
                                                         #:punctuation punctuation
                                                         #:syllable-first-tone-class syllable-first-tone-class
                                                         #:syllable-second-tone-class syllable-second-tone-class
                                                         #:syllable-third-tone-class syllable-third-tone-class
                                                         #:syllable-fourth-tone-class syllable-fourth-tone-class
                                                         #:syllable-neutral-tone-class syllable-neutral-tone-class
                                                         str)])
    (and fragment
         `(,tag (,@(attr 'class class)
                 ,@(attr 'lang lang))
                ,@fragment))))
