#lang racket/base

(provide attr
         string->html-fragment
         html-fragment->html)

(require (only-in racket/function
                  const)

         pin1yin1/parse
         (submod pin1yin1/parse char))

(define (attr name value)
  (if value
      `((,name ,value))
      '()))

(define (string->html-fragment #:class [class #f]
                               #:lang [lang #f]
                               str)
  (let ([content
         (parse-string!
          (multi/p (or/p (map/p (const #x200B) (eq/p #\u200B)) ; Zero-width space
                         (map/p (const #x3000) (eq/p #\u3000)) ; Fullwidth (ideographic) space
                         (map/p (const 'Tab) (eq/p #\tab))
                         (map/p (const 'NewLine) newline/p)
                         (map/p list->string
                                (multi+/p (right/p (not/p null
                                                          (or/p (eq/p #\u200B)
                                                                (eq/p #\u3000)
                                                                (eq/p #\tab)
                                                                newline/p))
                                                   any/p)))))
          str)])
    (if (or class lang)
        `((span (,@(attr 'class class)
                 ,@(attr 'lang lang))
                ,@content))
        content)))

(define (html-fragment->html #:tag tag
                             #:class [class #f]
                             #:lang [lang #f]
                             fragment)
  `(,tag (,@(attr 'class class)
          ,@(attr 'lang lang))
         ,@fragment))
