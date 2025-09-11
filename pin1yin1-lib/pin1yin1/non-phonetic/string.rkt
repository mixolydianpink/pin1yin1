#lang racket/base

(provide ->punctuation->string)

(require racket/match
         (only-in racket/string
                  string-append*)

         (submod pin1yin1/non-phonetic internal))

(define (->punctuation->string sym)
  (case sym
    [(zh-Latn) punctuation->zh-Latn]
    [(zh-Latn+fullwidth-space) (+fullspace punctuation->zh-Latn)]
    [(zh-TW) punctuation->zh-TW]
    [(zh-TW+space) (+space punctuation->zh-TW)]
    [(zh-TW+space/zero-width) (+space #:space "\u200B" punctuation->zh-TW)]
    [(zh-CN) punctuation->zh-CN]
    [(zh-CN+space) (+space punctuation->zh-CN)]
    [(zh-CN+space/zero-width) (+space #:space "\u200B" punctuation->zh-CN)]))

(define (punctuation->zh-Latn punctuation)
  (match (assoc punctuation punctuation-table)
    [(list* _ _ zh-Latn _)
     zh-Latn]
    [#f
     (error "Unrecognized Chinese (Latn) whitespace/punctuation: "
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

(define (+space #:space [space " "]
                punctuation->string)
  (λ (punctuation)
    (if (equal? 'space punctuation)
        space
        (punctuation->string punctuation))))

(define (+fullspace #:fullspace [fullspace "\u3000"]
                    punctuation->string)
  (λ (punctuation)
    (if (equal? 'fullspace punctuation)
        fullspace
        (punctuation->string punctuation))))
