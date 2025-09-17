#lang racket/base

(provide pin1yin1->html-fragment)

(require pin1yin1/pin1yin1)

(define (pin1yin1->html-fragment #:compound->html-fragment compound->html-fragment
                                 #:non-phonetic->html-fragment non-phonetic->html-fragment
                                 pin1yin1)
  (pin1yin1-append-map #:compound->list compound->html-fragment
                       #:non-phonetic->list non-phonetic->html-fragment
                       pin1yin1))
