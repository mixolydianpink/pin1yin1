#lang racket/base

(provide capitalize

         char-capital/halfwidth?
         char-numeral/halfwidth?

         string/fullwidth-capitals-and-numerals)

(require (only-in racket/list
                  index-of))

(define (capitalize str)
  (if (equal? "" str)
      ""
      (string-append (string (char-upcase (string-ref str 0)))
                     (substring str 1))))

(define capitals/halfwidth
  (string->list "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(define capitals/fullwidth
  (string->list "ＡＢＣＤＥＦＧＨＩＪＫＬＭＮＯＰＱＲＳＴＵＶＷＸＹＺ"))

(define numerals/halfwidth
  (string->list "0123456789"))

(define numerals/fullwidth
  (string->list "０１２３４５６７８９"))

(define (char-capital/halfwidth? char)
  (if (member char capitals/halfwidth char=?) #t #f))

(define (char-numeral/halfwidth? char)
  (if (member char numerals/halfwidth char=?) #t #f))

(define (string/fullwidth-capitals-and-numerals string)
  (define (string-map f string)
    (list->string (map f (string->list string))))
  (string-map (λ (char)
                (cond
                  [(index-of capitals/halfwidth char)
                   => (λ (index) (list-ref capitals/fullwidth index))]
                  [(index-of numerals/halfwidth char)
                   => (λ (index) (list-ref numerals/fullwidth index))]
                  [else
                   char]))
              string))
