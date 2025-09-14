#lang racket/base

(provide (struct-out literal)

         whitespace?
         punctuation?
         non-phonetic?

         make-whitespace->
         non-phonetic->)

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
    [explicit-space          . ,(eq/p #\_)]
    [zero-width-space        . ,(eq/p #\# #\z #\#)]
    [fullwidth-space         . ,(eq/p #\# #\s #\#)]
    [tab                     . ,(or/p (eq/p #\tab)
                                      (eq/p #\# #\t #\#))]
    [newline                 . ,(or/p newline/p
                                      (eq/p #\# #\n #\#))]))

(define (whitespace? sym)
  (if (assoc sym whitespace-table)
      #t
      #f))

(define (make-whitespace-> #:space space
                           #:explicit-space explicit-space
                           #:zero-width-space zero-width-space
                           #:fullwidth-space fullwidth-space
                           #:tab tab
                           #:newline newline)
  (match-λ
   ['space space]
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
    [em-dash                  ,(eq/p #\- #\- #\-)          "—"    "⸺"    "⸺"]
    [wave-dash                ,(eq/p #\~)                  "~"    "～"    "～"]
    [left-parenthesis         ,(eq/p #\()                  "("    "（"    "（"]
    [right-parenthesis        ,(eq/p #\))                  ")"    "）"    "）"]
    [left-outer-quote         ,(eq/p #\[)                  "“"    "「"    "“"]
    [right-outer-quote        ,(eq/p #\])                  "”"    "」"    "”"]
    [left-inner-quote         ,(eq/p #\{)                  "‘"    "『"    "‘"]
    [right-inner-quote        ,(eq/p #\})                  "’"    "』"    "’"]))

(define (punctuation? sym)
  (if (assoc sym punctuation-table)
      #t
      #f))

(define non-phonetic? (disjoin literal?
                               whitespace?
                               punctuation?))

(define (non-phonetic-> #:literal-> literal->
                        #:whitespace-> whitespace->
                        #:punctuation-> punctuation->
                        non-phonetic)
  (match non-phonetic
    [(? literal? literal)
     (literal-> literal)]
    [(? whitespace? whitespace)
     (whitespace-> whitespace)]
    [(? punctuation? punctuation)
     (punctuation-> punctuation)]))
