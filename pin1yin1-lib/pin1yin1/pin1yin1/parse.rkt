#lang racket/base

(provide pin1yin1/p
         make-pin1yin1/p)

(require pin1yin1/list
         pin1yin1/parse
         pin1yin1/phonetic/parse)

(define (pin1yin1/p #:complex/p complex/p
                    #:non-phonetic/p non-phonetic/p)
  (map/p flatten1
         (alternating-symmetric/p complex/p
                                  non-phonetic/p)))

(define (make-pin1yin1/p #:implicit-neutral-tone? implicit-neutral-tone?
                         #:interpret-v-as-u-umlaut? interpret-v-as-u-umlaut?
                         #:interpret-e^-as-e-circumflex? interpret-e^-as-e-circumflex?
                         #:non-phonetic/p non-phonetic/p)
  (pin1yin1/p #:complex/p (make-complex/p #:implicit-neutral-tone? implicit-neutral-tone?
                                          #:interpret-v-as-u-umlaut? interpret-v-as-u-umlaut?
                                          #:interpret-e^-as-e-circumflex? interpret-e^-as-e-circumflex?)
              #:non-phonetic/p non-phonetic/p))
