#lang racket/base

(provide flatten1)

(require (only-in racket/list
                  append-map))

(define (flatten1 lst)
  (append-map (λ (any)
                (if (list? any)
                    any
                    (list any)))
              lst))
