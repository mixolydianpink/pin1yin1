#lang racket/base

(provide flatten1)

(require (only-in racket/list
                  append-map))

(define (->list any)
  (if (list? any)
      any
      (list any)))

(define (flatten1 list)
  (append-map ->list
              list))
