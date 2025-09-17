#lang racket/base

(provide pin1yin1?

         pin1yin1-map
         pin1yin1-append-map)

(require (only-in racket/function
                  conjoin
                  disjoin)
         (only-in racket/list
                  append-map)
         racket/match

         pin1yin1/non-phonetic
         pin1yin1/phonetic)

(define (pin1yin1? any)
  ((conjoin list?
            (λ (list)
              (andmap (disjoin compound?
                               non-phonetic?)
                      list)))
   any))

(define (pin1yin1-map #:compound-> compound->
                      #:non-phonetic-> non-phonetic->
                      pin1yin1)
  (map (match-λ [(? compound? compound)
                 (compound-> compound)]
                [(? non-phonetic? non-phonetic)
                 (non-phonetic-> non-phonetic)])
       pin1yin1))

(define (pin1yin1-append-map #:compound->list compound->list
                             #:non-phonetic->list non-phonetic->list
                             pin1yin1)
  (append-map (match-λ [(? compound? compound)
                        (compound->list compound)]
                       [(? non-phonetic? non-phonetic)
                        (non-phonetic->list non-phonetic)])
              pin1yin1))
