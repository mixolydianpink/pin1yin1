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
                                 #:explicit-neutral-tone? [explicit-neutral-tone? #f]
                                 #:punctuation [punctuation 'en]
                                 str)
  (parse-string! (or/p (left/p (apply/p (pure/p (curry pin1yin1->string
                                                       #:complex->string
                                                       (curry complex->pinyin
                                                              #:explicit-neutral-tone? explicit-neutral-tone?)
                                                       #:non-phonetic->string
                                                       (curry non-phonetic->
                                                              #:literal-> literal-content
                                                              #:punctuation->
                                                              (->punctuation->string punctuation))))
                                        (make-pin1yin1/p #:implicit-neutral-tone?
                                                         implicit-neutral-tone?
                                                         #:non-phonetic/p non-phonetic/p))
                               (eos/p null))
                       (pure/p #f))
                 str))

(define (pin1yin1-string->zhuyin #:implicit-neutral-tone? [implicit-neutral-tone? #f]
                                 #:explicit-first-tone? [explicit-first-tone? #f]
                                 #:prefix-neutral-tone? [prefix-neutral-tone? #f]
                                 #:punctuation [punctuation 'zh-TW]
                                 str)
  (parse-string! (or/p (left/p (apply/p (pure/p (curry pin1yin1->string
                                                       #:complex->string
                                                       (curry complex->zhuyin
                                                              #:explicit-first-tone? explicit-first-tone?
                                                              #:prefix-neutral-tone? prefix-neutral-tone?)
                                                       #:non-phonetic->string
                                                       (curry non-phonetic->
                                                              #:literal-> literal-content
                                                              #:punctuation->
                                                              (->punctuation->string punctuation))))
                                        (make-pin1yin1/p #:implicit-neutral-tone?
                                                         implicit-neutral-tone?
                                                         #:non-phonetic/p non-phonetic/p))
                               (eos/p null))
                       (pure/p #f))
                 str))
