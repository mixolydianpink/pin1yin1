#lang racket/base

(provide syllable->pinyin/span
         syllable->zhuyin/span
         polysyllable->pinyin/html-fragment
         polysyllable->zhuyin/html-fragment
         compound->html-fragment)

(require (only-in racket/list
                  add-between
                  append-map
                  empty?
                  list-prefix?)
         racket/match

         pin1yin1/list
         pin1yin1/markup
         pin1yin1/phonetic
         pin1yin1/string)

(define (syllable->pinyin/span #:diacritic-e^? diacritic-e^?
                               #:diacritic-m? diacritic-m?
                               #:diacritic-n? diacritic-n?
                               #:diacritic-ng? diacritic-ng?
                               #:explicit-neutral-tone? explicit-neutral-tone?
                               #:first-tone-class first-tone-class
                               #:second-tone-class second-tone-class
                               #:third-tone-class third-tone-class
                               #:fourth-tone-class fourth-tone-class
                               #:neutral-tone-class neutral-tone-class
                               #:suppress-leading-apostrophe? suppress-leading-apostrophe?
                               syllable)
  (define (syllable->pin1yin1/html-fragment syllable)
    (let ([segments (list->string (syllable-segments syllable))]
          [erization/html-fragment
           (case (syllable-erization syllable)
             [(bare) (if (equal? '(#\e) (syllable-segments syllable))
                         '("'r")
                         '("r"))]
             [(parenthesized) '("(r)")]
             [(none) '()])]
          [tone
           (case (syllable-tone syllable)
             [(0) "5"]
             [(1) "1"]
             [(2) "2"]
             [(3) "3"]
             [(4) "4"])])
      `((span ()
              ,(string-append (if (syllable-capitalized? syllable)
                                  (capitalize segments)
                                  segments))
              ,@(if (empty? erization/html-fragment)
                    '()
                    `((span ()
                            ,@erization/html-fragment))))
        (span ()
              ,tone))))
  (let ([class (case (syllable-tone syllable)
                 [(0) neutral-tone-class]
                 [(1) first-tone-class]
                 [(2) second-tone-class]
                 [(3) third-tone-class]
                 [(4) fourth-tone-class])])
    (match-let ([(list pre unmarked post) (syllable-segments/grouped syllable)])
      (let* ([neutral-tone? (= 0 (syllable-tone syllable))]
             [numbered?
              (and (not neutral-tone?)
                   (case unmarked
                     [((#\ê)) (not diacritic-e^?)]
                     [((#\m)) (not diacritic-m?)]
                     [((#\n)) (or (and (not diacritic-n?)
                                       (not (list-prefix? '(#\g) post)))
                                  (and (not diacritic-ng?)
                                       (list-prefix? '(#\g) post)))]
                     [else #f]))]
             [pinyin/html-fragment
              (if numbered?
                  (syllable->pin1yin1/html-fragment syllable)
                  `(,(string-append (if (and explicit-neutral-tone? neutral-tone?)
                                        "·"
                                        "")
                                    (syllable-pinyin-core syllable)
                                    (case (syllable-erization syllable)
                                      [(bare) (if (equal? '(#\e) (syllable-segments syllable))
                                                  "'r"
                                                  "r")]
                                      [(parenthesized) "(r)"]
                                      [(none) ""]))))])
        (html-fragment->html #:tag 'span
                             #:class class
                             (flatten1 `(,(if (and (not suppress-leading-apostrophe?)
                                                   (or (empty? pre)
                                                       (let ([segments (syllable-segments syllable)])
                                                         (or (list-prefix? '(#\g #\n) segments)
                                                             (list-prefix? '(#\n #\g) segments)))))
                                              '("'")
                                              '())
                                         ,pinyin/html-fragment)))))))

(define (syllable->zhuyin/span #:syllabic-m? syllabic-m?
                               #:syllabic-n? syllabic-n?
                               #:syllabic-ng? syllabic-ng?
                               #:explicit-empty-rhyme? explicit-empty-rhyme?
                               #:explicit-first-tone? explicit-first-tone?
                               #:prefix-neutral-tone? prefix-neutral-tone?
                               #:first-tone-class first-tone-class
                               #:second-tone-class second-tone-class
                               #:third-tone-class third-tone-class
                               #:fourth-tone-class fourth-tone-class
                               #:neutral-tone-class neutral-tone-class
                               syllable)
  (let* ([core
          (syllable-zhuyin-core #:syllabic-m? syllabic-m?
                                #:syllabic-n? syllabic-n?
                                #:syllabic-ng? syllabic-ng?
                                #:explicit-empty-rhyme? explicit-empty-rhyme?
                                syllable)]
         [tone (syllable-tone syllable)]
         [class (case tone
                  [(0) neutral-tone-class]
                  [(1) first-tone-class]
                  [(2) second-tone-class]
                  [(3) third-tone-class]
                  [(4) fourth-tone-class])])
    (html-fragment->html #:tag 'span
                         #:class class
                         `(,(case tone
                              [(0)
                               (if prefix-neutral-tone?
                                   `(span ()
                                          (span ()
                                                ,(syllable-zhuyin-tone-mark syllable))
                                          ,core)
                                   `(span ()
                                          ,core
                                          (span ()
                                                ,(syllable-zhuyin-tone-mark syllable))))]
                              [(1)
                               (if explicit-first-tone?
                                   `(span ()
                                          ,core
                                          (span ()
                                                ,(syllable-zhuyin-tone-mark syllable)))
                                   `(span ()
                                          ,core))]
                              [(2 3 4)
                               `(span ()
                                      ,core
                                      (span ()
                                            ,(syllable-zhuyin-tone-mark syllable)))])
                           ,@(case (syllable-erization syllable)
                               [(bare) '("ㄦ")]
                               [(parenthesized) '("（ㄦ）")]
                               [(none) '()])))))

(define (polysyllable->pinyin/html-fragment #:sep/html-fragment sep/html-fragment
                                            #:syllable->pinyin/html-fragment syllable->pinyin/html-fragment
                                            polysyllable)
  (match-let ([(cons first rest) (polysyllable-syllables polysyllable)])
    (flatten1 (cons (syllable->pinyin/html-fragment #:suppress-leading-apostrophe? #t
                                                    first)
                    (for/list ([syllable rest])
                      (append sep/html-fragment
                              (syllable->pinyin/html-fragment #:suppress-leading-apostrophe? #f
                                                              syllable)))))))

(define (polysyllable->zhuyin/html-fragment #:sep/html-fragment sep/html-fragment
                                            #:syllable->zhuyin/html-fragment syllable->zhuyin/html-fragment
                                            polysyllable)
  (match-let ([(cons first rest) (polysyllable-syllables polysyllable)])
    (flatten1 (cons (syllable->zhuyin/html-fragment first)
                    (for/list ([syllable rest])
                      (append sep/html-fragment
                              (syllable->zhuyin/html-fragment syllable)))))))

(define (compound->html-fragment #:sep/html-fragment sep/html-fragment
                                 #:polysyllable->html-fragment polysyllable->html-fragment
                                 #:string->html-fragment string->html-fragment
                                 compound)
  (flatten1 (add-between (for/list ([polysyllable-or-string (compound-polysyllables-and-strings compound)])
                           (match polysyllable-or-string
                             [(? polysyllable? polysyllable)
                              (polysyllable->html-fragment polysyllable)]
                             [(? string? string)
                              (string->html-fragment string)]))
                         sep/html-fragment)))
