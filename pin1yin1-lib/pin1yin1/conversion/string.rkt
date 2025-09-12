#lang racket/base

(provide pin1yin1-string->pinyin
         pin1yin1-string->zhuyin)

(require (only-in racket/function
                  curry)

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
  (parse-string! (or/p (left/p (apply/p (pure/p (curry pin1yin1->string
                                                       #:compound->string
                                                       (curry compound->pinyin
                                                              #:explicit-neutral-tone? explicit-neutral-tone?)
                                                       #:non-phonetic->string
                                                       (curry non-phonetic->
                                                              #:literal-> literal-content
                                                              #:whitespace->
                                                              (->whitespace-> #:space
                                                                              (case space
                                                                                [(none) ""]
                                                                                [(zero-width) "\u200B"]
                                                                                [(halfwidth) " "]
                                                                                [(fullwidth) "\u3000"])
                                                                              #:underscore " "
                                                                              #:zero-width-space "\u200B"
                                                                              #:fullwidth-space "\u3000"
                                                                              #:tab "\t"
                                                                              #:newline "\n")
                                                              #:punctuation->
                                                              (->punctuation->string punctuation))))
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
  (parse-string! (or/p (left/p (apply/p (pure/p (curry pin1yin1->string
                                                       #:compound->string
                                                       (curry compound->zhuyin
                                                              #:explicit-first-tone? explicit-first-tone?
                                                              #:prefix-neutral-tone? prefix-neutral-tone?
                                                              #:explicit-empty-rhyme? explicit-empty-rhyme?
                                                              #:syllabic-m? syllabic-m?
                                                              #:syllabic-n? syllabic-n?
                                                              #:syllabic-ng? syllabic-ng?)
                                                       #:non-phonetic->string
                                                       (curry non-phonetic->
                                                              #:literal-> literal-content
                                                              #:whitespace->
                                                              (->whitespace-> #:space
                                                                              (case space
                                                                                [(none) ""]
                                                                                [(zero-width) "\u200B"]
                                                                                [(halfwidth) " "]
                                                                                [(fullwidth) "\u3000"])
                                                                              #:underscore " "
                                                                              #:zero-width-space "\u200B"
                                                                              #:fullwidth-space "\u3000"
                                                                              #:tab "\t"
                                                                              #:newline "\n")
                                                              #:punctuation->
                                                              (->punctuation->string punctuation))))
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
