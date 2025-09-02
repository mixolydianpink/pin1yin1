#lang racket/base

(provide literal->html-fragment)

(require racket/match

         pin1yin1/markup
         pin1yin1/non-phonetic
         pin1yin1/option)

(define literal->html-fragment
  (match-λ
   [(literal (some lang) content)
    `((span (,@(attr 'lang lang))
            ,@(string->html-fragment content)))]
   [(literal (none) content)
    (string->html-fragment content)]))
