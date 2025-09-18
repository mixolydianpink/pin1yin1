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
  (let* ([literal-or-empty/p
          (bind/p (match-λ
                   [(some "")
                    never/p]
                   [lang
                    (bind/p (λ (content)
                              (if (and (none? lang)
                                       (equal? "" content))
                                  (pure/p empty) ; Will be `flatten1`ed out.
                                  (pure/p (literal lang content))))
                            (map/p list->string
                                   (delimited/p #:pre '(#\|)
                                                #:post '(#\|)
                                                #:escapable? #t
                                                (if/p char?))))])
                  (opt/p (map/p list->string
                                (between/p #:pre/p (eq/p #\#)
                                           #:post/p (lookahead/p (eq/p #\|))
                                           (right/p (not/p null
                                                           (eq/p #\#))
                                                    (if/p char?))))))]
         [whitespace/p
          (apply or/p (for/list ([row whitespace-table])
                        (match-let ([(cons symbol parser) row])
                          (map/p (const symbol) parser))))]
         [punctuation/p
          (apply or/p (for/list ([row punctuation-table])
                        (match-let ([(list* symbol parser _) row])
                          (map/p (const symbol) parser))))])
    (map/p flatten1
           (multi+/p (or/p literal-or-empty/p
                           whitespace/p
                           punctuation/p)))))
