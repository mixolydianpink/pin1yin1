#lang racket/base

(provide pin1yin1/p)

(require (only-in racket/function
                  curry)

         pin1yin1/list
         pin1yin1/parse)

(define (pin1yin1/p #:compound/p compound/p
                    #:non-phonetic/p non-phonetic/p)
  (map/p flatten1
         (alternating-symmetric/p compound/p
                                  non-phonetic/p)))
