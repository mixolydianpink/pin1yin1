#lang racket/base

(provide ->punctuation->string)

(require racket/match
         (only-in racket/string
                  string-append*)

         (submod pin1yin1/non-phonetic internal))

(define (->punctuation->string sym)
  (λ (punctuation)
    (string-append* (map (case sym
                           [(en) punctuation->en]
                           [(zh-TW) punctuation->zh-TW]
                           [(zh-TW+space) (+space punctuation->zh-TW)]
                           [(zh-CN) punctuation->zh-CN]
                           [(zh-CN+space) (+space punctuation->zh-CN)])
                         punctuation))))

(define (punctuation->en punctuation)
  (match (assoc punctuation punctuation-table)
    [(list* _ _ en _)
     en]
    [#f
     (error "Unrecognized English whitespace/punctuation: "
            punctuation)]))

(define (punctuation->zh-TW punctuation)
  (match (assoc punctuation punctuation-table)
    [(list* _ _ _ zh-TW _)
     zh-TW]
    [#f
     (error "Unrecognized Chinese (TW) whitespace/punctuation: "
            punctuation)]))

(define (punctuation->zh-CN punctuation)
  (match (assoc punctuation punctuation-table)
    [(list* _ _ _ _ zh-CN _)
     zh-CN]
    [#f
     (error "Unrecognized Chinese (CN) whitespace/punctuation: "
            punctuation)]))

(define (+space punctuation->string)
  (λ (punctuation)
    (if (equal? 'space punctuation)
        " "
        (punctuation->string punctuation))))
