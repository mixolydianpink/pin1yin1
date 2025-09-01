#lang racket/base

(provide pst/p)

(require (only-in racket/list
                  empty)
         racket/match

         pin1yin1/option
         pin1yin1/parse
         pin1yin1/pst)

(define (pst/p #:pst pst
               #:prefix [prefix empty]
               key/p)
  (let loop ([reverse-key-list (reverse prefix)])
    (let ([key-list (reverse reverse-key-list)])
      (let-values ([(value prefix?) (pst-ref/prefix? pst key-list)])
        (let ([result/p
               (match value
                 [(some value)
                  (pure/p (cons key-list value))]
                 [(none)
                  never/p])])
          (if (not prefix?)
              result/p
              (or/p (bind/p (λ (next-key)
                              (loop (cons next-key reverse-key-list)))
                            key/p)
                    result/p)))))))
