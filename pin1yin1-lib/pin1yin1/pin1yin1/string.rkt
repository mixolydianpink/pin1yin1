#lang racket/base

(provide pin1yin1->string
         make-pin1yin1->pinyin
         make-pin1yin1->zhuyin)

(require (only-in racket/function
                  curry)
         (only-in racket/string
                  string-append*)

         pin1yin1/non-phonetic
         pin1yin1/non-phonetic/string
         pin1yin1/phonetic/string
         pin1yin1/pin1yin1)

(define (pin1yin1->string #:compound->string compound->string
                          #:non-phonetic->string non-phonetic->string
                          pin1yin1)
  (string-append* (pin1yin1-map #:compound-> compound->string
                                #:non-phonetic-> non-phonetic->string
                                pin1yin1)))

(define (make-pin1yin1->pinyin #:explicit-neutral-tone? [explicit-neutral-tone? #f]
                               #:space [space 'halfwidth]
                               #:punctuation [punctuation 'zh-Latn])
  (λ (pin1yin1)
    (pin1yin1->string #:compound->string
                      (curry compound->pinyin
                             #:explicit-neutral-tone? explicit-neutral-tone?)
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
                                                #:underscore " "
                                                #:zero-width-space "\u200B"
                                                #:fullwidth-space "\u3000"
                                                #:tab "\t"
                                                #:newline "\n")
                             #:punctuation->
                             (->punctuation->string punctuation))
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
                      (curry compound->zhuyin
                             #:syllabic-m? syllabic-m?
                             #:syllabic-n? syllabic-n?
                             #:syllabic-ng? syllabic-ng?
                             #:explicit-empty-rhyme? explicit-empty-rhyme?
                             #:explicit-first-tone? explicit-first-tone?
                             #:prefix-neutral-tone? prefix-neutral-tone?)
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
                                                #:underscore " "
                                                #:zero-width-space "\u200B"
                                                #:fullwidth-space "\u3000"
                                                #:tab "\t"
                                                #:newline "\n")
                             #:punctuation->
                             (->punctuation->string punctuation))
                      pin1yin1)))
