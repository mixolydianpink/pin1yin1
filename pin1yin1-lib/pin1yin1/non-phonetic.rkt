#lang racket/base

(provide (struct-out literal)

         whitespace-symbol?
         punctuation-symbol?
         non-phonetic?

         make-whitespace->
         make-non-phonetic->)

(module+ internal
  (provide whitespace-table
           punctuation-table))

(require (only-in racket/function
                  disjoin)
         racket/match

         pin1yin1/parse
         (submod pin1yin1/parse char))

(struct literal (lang content) #:transparent)

(define whitespace-table
  `([space                   . ,(eq/p #\space)]
    [underscore              . ,(eq/p #\_)]
    [explicit-space          . ,(eq/p #\# #\s #\#)]
    [zero-width-space        . ,(eq/p #\# #\z #\#)]
    [fullwidth-space         . ,(eq/p #\# #\f #\#)]
    [tab                     . ,(or/p (eq/p #\tab)
                                      (eq/p #\# #\t #\#))]
    [newline                 . ,(or/p newline/p
                                      (eq/p #\# #\n #\#))]))

(define (whitespace-symbol? any)
  (match any
    [(? symbol? symbol) #:when (assoc symbol whitespace-table)
                        #t]
    [_ #f]))

(define (make-whitespace-> #:space space
                           #:underscore underscore
                           #:explicit-space explicit-space
                           #:zero-width-space zero-width-space
                           #:fullwidth-space fullwidth-space
                           #:tab tab
                           #:newline newline)
  (match-λ
   ['space space]
   ['underscore underscore]
   ['explicit-space explicit-space]
   ['zero-width-space zero-width-space]
   ['fullwidth-space fullwidth-space]
   ['tab tab]
   ['newline newline]))

(define punctuation-table
  `([ellipsis                 ,(or/p (eq/p #\…)
                                     (eq/p #\. #\. #\.))   "…"    "⋯⋯"   "⋯⋯"]
    [stop                     ,(eq/p #\.)                  "."    "。"    "。"]
    [bang                     ,(eq/p #\!)                  "!"    "！"    "！"]
    [question                 ,(eq/p #\?)                  "?"    "？"    "？"]
    [comma                    ,(eq/p #\,)                  ","    "，"    "，"]
    [dun                      ,(eq/p #\\)                  ","    "、"    "、"]
    [colon                    ,(eq/p #\:)                  ":"    "："    "："]
    [semicolon                ,(eq/p #\;)                  ";"    "；"    "；"]
    [slash                    ,(eq/p #\/)                  "/"    "/"    "/"]
    [em-dash                  ,(or/p (eq/p #\—)
                                     (eq/p #\- #\- #\-))   "—"    "⸺"    "⸺"]
    [wave-dash                ,(eq/p #\~)                  "~"    "～"    "～"]
    [left-parenthesis         ,(eq/p #\()                  "("    "（"    "（"]
    [right-parenthesis        ,(eq/p #\))                  ")"    "）"    "）"]
    [left-outer-quote         ,(or/p (eq/p #\“)
                                     (eq/p #\[))           "“"    "「"    "“"]
    [right-outer-quote        ,(or/p (eq/p #\”)
                                     (eq/p #\]))           "”"    "」"    "”"]
    [left-inner-quote         ,(or/p (eq/p #\‘)
                                     (eq/p #\{))           "‘"    "『"    "‘"]
    [right-inner-quote        ,(or/p (eq/p #\’)
                                     (eq/p #\}))           "’"    "』"    "’"]))

(define (punctuation-symbol? any)
  (match any
    [(? symbol? symbol) #:when (assoc symbol punctuation-table)
                        #t]
    [_ #f]))

(define (non-phonetic? any)
  ((disjoin literal?
            whitespace-symbol?
            punctuation-symbol?)
   any))

(define (make-non-phonetic-> #:literal-> literal->
                             #:whitespace-> whitespace->
                             #:punctuation-> punctuation->)
  (match-λ
   [(? literal? literal)
    (literal-> literal)]
   [(? whitespace-symbol? whitespace-symbol)
    (whitespace-> whitespace-symbol)]
   [(? punctuation-symbol? punctuation-symbol)
    (punctuation-> punctuation-symbol)]))
