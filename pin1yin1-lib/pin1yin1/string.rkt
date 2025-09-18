#lang racket/base

(provide capitalize)

(define (capitalize str)
  (if (equal? "" str)
      ""
      (string-append (string (char-upcase (string-ref str 0)))
                     (substring str 1))))
