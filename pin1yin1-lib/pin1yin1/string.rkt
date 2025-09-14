#lang racket/base

(provide capitalize)

(require racket/match)

(define (capitalize string)
  (match (string->list string)
    [(cons first rest)
     (list->string (cons (char-upcase first) rest))]
    [empty
     ""]))
