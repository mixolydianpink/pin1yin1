#lang racket/base

(provide (struct-out literal)

         non-phonetic->)

(module+ internal
  (provide punctuation-table))

(require racket/match

         pin1yin1/parse
         (submod pin1yin1/parse char))

(struct literal (lang content) #:transparent)

(define punctuation-table
  (let ([zwsp "\u200B"]  ; Zero-width space
        [fwsp "\u3000"]) ; Fullwidth (ideographic) space
    `((space        ,(eq/p #\space)            " "    ""     "")
      (explspace    ,(eq/p #\_)                " "    " "    " ")
      (zerospace    ,(eq/p #\# #\z #\#)        ,zwsp  ,zwsp  ,zwsp)
      (fullspace    ,(eq/p #\# #\s #\#)        " "    ,fwsp  ,fwsp)
      (tab          ,(or/p (eq/p #\tab)
                           (eq/p #\# #\t #\#)) "\t"   "\t"   "\t")
      (newline      ,(or/p newline/p
                           (eq/p #\# #\n #\#)) "\n"   "\n"   "\n")
      (ellipsis     ,(or/p (eq/p #\…)
                           (eq/p #\. #\. #\.)) "…"    "⋯⋯"   "⋯⋯")
      (stop         ,(eq/p #\.)                "."    "。"    "。")
      (bang         ,(eq/p #\!)                "!"    "！"    "！")
      (question     ,(eq/p #\?)                "?"    "？"    "？")
      (comma        ,(eq/p #\,)                ","    "，"    "，")
      (enum         ,(eq/p #\\)                ","    "、"    "、")
      (colon        ,(eq/p #\:)                ":"    "："    "：")
      (semicolon    ,(eq/p #\;)                ";"    "；"    "；")
      (slash        ,(eq/p #\/)                "/"    "/"    "/")
      (emdash       ,(eq/p #\- #\- #\-)        "—"    "⸺"    "⸺")
      (wave         ,(eq/p #\~)                "~"    "～"    "～")
      (lparen       ,(eq/p #\()                "("    "（"    "（")
      (rparen       ,(eq/p #\))                ")"    "）"    "）")
      (loutquote    ,(eq/p #\[)                "“"    "「"    "“")
      (routquote    ,(eq/p #\])                "”"    "」"    "”")
      (linquote     ,(eq/p #\{)                "‘"    "『"    "‘")
      (rinquote     ,(eq/p #\})                "’"    "』"    "’"))))

(define (non-phonetic-> #:literal-> literal->
                        #:punctuation-> punctuation->
                        non-phonetic)
  (match non-phonetic
    [(? literal? literal)
     (literal-> literal)]
    [(? list? punctuation)
     (punctuation-> punctuation)]))
