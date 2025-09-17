#lang racket/base

(provide literal->html-fragment)

(require racket/match

         pin1yin1/markup
         pin1yin1/non-phonetic
         pin1yin1/option)

(define (literal->html-fragment literal)
  (string->html-fragment #:lang
                         (match (literal-lang literal)
                           [(some lang)
                            lang]
                           [(none)
                            #f])
                         (literal-content literal)))
