#lang racket/base

(provide pin1yin1->string)

(require (only-in racket/string
                  string-append*)

         pin1yin1/pin1yin1)

(define (pin1yin1->string #:complex->string complex->string
                          #:non-phonetic->string non-phonetic->string
                          pin1yin1)
  (string-append* (pin1yin1-map #:complex-> complex->string
                                #:non-phonetic-> non-phonetic->string
                                pin1yin1)))
