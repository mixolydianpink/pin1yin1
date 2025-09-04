#lang racket/base

(provide attr
         string->html-fragment)

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
          (multi/p (or/p (map/p (const 'NewLine) newline/p)
                         (map/p (const 'Tab) (eq/p #\tab))
                         (map/p (const 'ZeroWidthSpace) (eq/p #\u200B))
                         (map/p (const #x3000) (eq/p #\u3000)) ; Full-width space
                         (map/p list->string
                                (multi+/p (right/p (not/p null
                                                          (or/p newline/p
                                                                (eq/p #\tab)
                                                                (eq/p #\u200B)
                                                                (eq/p #\u3000)))
                                                   any/p)))))
          str)])
    (if (or class lang)
        `((span (,@(attr 'class class)
                 ,@(attr 'lang lang))
                ,@content))
        content)))
