#lang racket/base

(provide ->punctuation->string)

(require racket/match

         (submod pin1yin1/non-phonetic internal))

(define (->punctuation->string symbol)
  (case symbol
    [(zh-Latn) punctuation->zh-Latn]
    [(zh-TW) punctuation->zh-TW]
    [(zh-CN) punctuation->zh-CN]))

(define (punctuation->zh-Latn punctuation)
  (match (assoc punctuation punctuation-table)
    [(list* _ _ zh-Latn _)
     zh-Latn]
    [#f
     (error "Unrecognized punctuation: "
            punctuation)]))

(define (punctuation->zh-TW punctuation)
  (match (assoc punctuation punctuation-table)
    [(list* _ _ _ zh-TW _)
     zh-TW]
    [#f
     (error "Unrecognized punctuation: "
            punctuation)]))

(define (punctuation->zh-CN punctuation)
  (match (assoc punctuation punctuation-table)
    [(list* _ _ _ _ zh-CN _)
     zh-CN]
    [#f
     (error "Unrecognized punctuation: "
            punctuation)]))
