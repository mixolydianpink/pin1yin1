#lang racket/base

(provide string->pinput
         list->pinput

         (struct-out parse-success)
         (struct-out parse-failure)

         parse-result->option

         parse
         parse!
         parse-string
         parse-string!
         parse-list
         parse-list!

         lookahead/p

         if/p
         not/p
         or/p
         then/p
         left/p
         right/p
         seq/p
         multi/p
         multi+/p
         sep-by/p
         alternating/p
         alternating-symmetric/p
         alternating+/p
         alternating-symmetric+/p
         between/p
         delimited/p

         map-partial/p
         map/p
         apply/p
         bind/p

         pure/p
         never/p
         opt/p
         any/p
         eq/p
         one-of/p
         eos/p

         ->list/p)

(module+ input
  (provide gen:pinput
           pinput?
           advance))

(module+ char
  (provide newline/p)

  (define newline/p
    (map/p (const #\newline)
           (or/p (eq/p #\return
                       #\newline)
                 (eq/p #\return)
                 (eq/p #\newline)))))

(require (only-in racket/function
                  const
                  curry)
         racket/generic
         (only-in racket/list
                  append*
                  empty)
         racket/match

         pin1yin1/option)

(define-generics pinput
  (advance pinput))

(struct string-pinput (str pos)
  #:methods gen:pinput
  [(define (advance pinput)
     (match-let ([(string-pinput str pos) pinput])
       (if (<= 0 pos (sub1 (string-length str)))
           (cons (some (string-ref str pos))
                 (string-pinput str (add1 pos)))
           (cons (none)
                 pinput))))])

(define (string->pinput str)
  (string-pinput str 0))

(struct list-pinput (preverse rest)
  #:methods gen:pinput
  [(define (advance pinput)
     (match-let ([(list-pinput preverse rest) pinput])
       (match rest
         [(cons next rest)
          (cons (some next)
                (list-pinput (cons next preverse) rest))]
         [empty
          (cons (none)
                pinput)])))])

(define (list->pinput lst)
  (list-pinput empty lst))

(struct parse-success (value rest) #:prefab)
(struct parse-failure () #:prefab)

(define (parse-result->option result)
  (match result
    [(parse-success value _)
     (some value)]
    [(? parse-failure?)
     (none)]))

(define (parse parser pinput)
  (parser pinput))

(define (parse! parser pinput)
  (match (parser pinput)
    [(parse-success value _)
     value]
    [(? parse-failure)
     (error "Parse error")]))

(define (parse-string parser str)
  (parse parser (string->pinput str)))

(define (parse-string! parser str)
  (parse! parser (string->pinput str)))

(define (parse-list parser lst)
  (parse parser (list->pinput lst)))

(define (parse-list! parser lst)
  (parse! parser (list->pinput lst)))

(define (lookahead/p parser)
  (λ (pinput)
    (match (parse parser pinput)
      [(parse-success value _)
       (parse-success value pinput)]
      [(? parse-failure? failure)
       failure])))

(define (if/p predicate?)
  (λ (pinput)
    (match (advance pinput)
      [(cons (some next) rest)
       (if (predicate? next)
           (parse-success next rest)
           (parse-failure))]
      [(cons (none) _)
       (parse-failure)])))

(define (not/p value parser)
  (λ (pinput)
    (match (parse parser pinput)
      [(? parse-success?)
       (parse-failure)]
      [(? parse-failure?)
       (parse-success value pinput)])))

(define (or/p first/p . alt/ps)
  (define ((else/p a/p b/p) pinput)
    (match (parse a/p pinput)
      [(? parse-success? success)
       success]
      [(? parse-failure?)
       (parse b/p pinput)]))
  (foldl (λ (a/p b/p)
           (else/p b/p a/p))
         first/p
         alt/ps))

(define (then/p a/p b/p)
  (λ (pinput)
    (match (parse a/p pinput)
      [(parse-success a rest)
       (parse (map/p (curry cons a) b/p) rest)]
      [(? parse-failure? failure)
       failure])))

(define (left/p a/p b/p)
  (map/p car (then/p a/p b/p)))

(define (right/p a/p b/p)
  (map/p cdr (then/p a/p b/p)))

(define (seq/p first/p . subsequent/ps)
  (λ (pinput)
    (let ([parsers (cons first/p subsequent/ps)])
      (let loop ([subsequent/ps parsers]
                 [rest pinput]
                 [reverse-values empty])
        (match subsequent/ps
          [(cons next/p subsequent/ps)
           (match (parse next/p rest)
             [(parse-success value rest)
              (loop subsequent/ps rest (cons value reverse-values))]
             [(? parse-failure? failure)
              failure])]
          [empty
           (parse-success (reverse reverse-values) rest)])))))

(define (multi/p parser)
  (λ (pinput)
    (let loop ([rest pinput]
               [reverse-values empty])
      (match (parse parser rest)
        [(parse-success value rest)
         (loop rest (cons value reverse-values))]
        [(? parse-failure?)
         (parse-success (reverse reverse-values) rest)]))))

(define (multi+/p parser)
  (then/p parser (multi/p parser)))

(define (sep-by/p #:sep/p sep/p
                  parser)
  (then/p parser
          (multi/p (right/p sep/p
                            parser))))

(define (alternating/p a/p b/p)
  (let loop ([a/p a/p]
             [b/p b/p]
             [reverse-values empty])
    (or/p (bind/p (λ (a)
                    (loop b/p a/p (cons a reverse-values)))
                  a/p)
          (map/p reverse
                 (pure/p reverse-values)))))

(define (alternating-symmetric/p a/p b/p)
  (bind/p (λ (a)
            (match a
              [(some a)
               (map/p (curry cons a)
                      (alternating/p b/p
                                     a/p))]
              [(none)
               (alternating/p b/p
                              a/p)]))
          (opt/p a/p)))

(define (alternating+/p a/p b/p)
  (bind/p (λ (a)
            (map/p (curry cons a)
                   (alternating/p b/p
                                  a/p)))
          a/p))

(define (alternating-symmetric+/p a/p b/p)
  (bind/p (λ (a)
            (match a
              [(some a)
               (map/p (curry cons a)
                      (alternating/p b/p
                                     a/p))]
              [(none)
               (alternating+/p b/p
                               a/p)]))
          (opt/p a/p)))

(define (between/p #:pre/p pre/p
                   #:post/p post/p
                   inner/p)
  (let ([bounded/p
         (multi/p (right/p (not/p null
                                  post/p)
                           inner/p))])
    (right/p pre/p
             (left/p bounded/p
                     post/p))))

(define (delimited/p #:pre pre
                     #:post post
                     #:escapable? [escapable? #f]
                     #:eq? [eq? equal?]
                     inner/p)
  (let ([eq/p (curry eq/p #:eq? eq?)])
    (let ([pre/p (apply eq/p pre)]
          [post/p (apply eq/p post)])
      (let ([bounded/p
             (map/p append*
                    (multi/p (or/p (if escapable?
                                       (map/p (const post)
                                              (then/p post/p post/p))
                                       never/p)
                                   (multi+/p (right/p (not/p null
                                                             post/p)
                                                      inner/p)))))])
        (right/p pre/p
                 (left/p bounded/p
                         post/p))))))

(define (map-partial/p f parser)
  (λ (pinput)
    (match (parse parser pinput)
      [(parse-success value rest)
       (match (f value)
         [(some value)
          (parse-success value rest)]
         [(none)
          (parse-failure)])]
      [(? parse-failure? failure)
       failure])))

(define (map/p f parser)
  (map-partial/p (compose some f) parser))

(define (apply/p f/p a/p)
  (bind/p (λ (f)
            (bind/p (λ (a)
                      (pure/p (f a)))
                    a/p))
          f/p))

(define (bind/p f parser)
  (λ (pinput)
    (match (parse parser pinput)
      [(parse-success value rest)
       (parse (f value) rest)]
      [(? parse-failure? failure)
       failure])))

(define (pure/p value)
  (λ (pinput)
    (parse-success value pinput)))

(define never/p
  (λ (pinput)
    (parse-failure)))

(define (opt/p parser)
  (or/p (map/p some
               parser)
        (pure/p (none))))

(define any/p
  (if/p (const #t)))

(define (eq/p #:eq? [eq? equal?]
              first
              . subsequents)
  (apply seq/p
         (map (λ (element)
                (if/p (curry eq? element)))
              (cons first subsequents))))

(define (one-of/p #:eq? [eq? equal?]
                  elements)
  (if/p (λ (input) (member input elements eq?))))

(define (eos/p value)
  (not/p value any/p))

(define ->list/p
  (multi/p any/p))
