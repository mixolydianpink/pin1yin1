#lang racket/base

(provide pin1yin1/p
         make-pin1yin1/p)

(require (only-in racket/function
                  curry)

         pin1yin1/list
         pin1yin1/non-phonetic/parse
         pin1yin1/parse
         pin1yin1/phonetic/parse)

(define (pin1yin1/p #:compound/p compound/p)
  (map/p flatten1
         (alternating-symmetric/p compound/p
                                  non-phonetic/p)))

(define (make-pin1yin1/p #:interpret-v-as-u-umlaut? interpret-v-as-u-umlaut?
                         #:interpret-e^-as-e-circumflex? interpret-e^-as-e-circumflex?
                         #:implicit-neutral-tone? implicit-neutral-tone?)
  (pin1yin1/p #:compound/p
              (compound/p (polysyllable/p #:implicit-neutral-tone? implicit-neutral-tone?
                                          (curry syllable/p
                                                 #:interpret-v-as-u-umlaut?
                                                 interpret-v-as-u-umlaut?
                                                 #:interpret-e^-as-e-circumflex?
                                                 interpret-e^-as-e-circumflex?)))))
