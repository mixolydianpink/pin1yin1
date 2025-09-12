#lang racket/base

(provide non-phonetic/p)

(require (only-in racket/function
                  const
                  curry)
         (only-in racket/list
                  empty)
         racket/match

         pin1yin1/list
         pin1yin1/non-phonetic
         (submod pin1yin1/non-phonetic internal)
         pin1yin1/option
         pin1yin1/parse)

(define non-phonetic/p
  (let* ([literal/p
          (bind/p (match-λ
                   [(some "")
                    never/p]
                   [lang
                    (map/p (compose (curry literal lang)
                                    list->string)
                           (delimited/p #:pre '(#\|)
                                        #:post '(#\|)
                                        #:escapable? #t
                                        (if/p char?)))])
                  (opt/p (map/p list->string
                                (between/p #:pre/p (eq/p #\#)
                                           #:post/p (lookahead/p (eq/p #\|))
                                           (right/p (not/p null
                                                           (eq/p #\#))
                                                    (if/p char?))))))]
         [literal-or-empty/p
          (map/p (match-λ
                  [(literal _ "")
                   empty]
                  [literal
                   literal])
                 literal/p)]
         [punctuation/p
          (apply or/p (for/list ([row punctuation-table])
                        (match-let ([(list* sym parser _) row])
                          (map/p (const sym) parser))))])
    (map/p flatten1
           (multi+/p (or/p literal-or-empty/p
                           punctuation/p)))))
