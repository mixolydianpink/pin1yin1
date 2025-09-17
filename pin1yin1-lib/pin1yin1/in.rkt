#lang racket/base

(provide pin1yin1-string->pin1yin1/rest
         pin1yin1-string->pin1yin1)

(require (only-in racket/function
                  curry)
         racket/match

         pin1yin1/non-phonetic/parse
         pin1yin1/parse
         pin1yin1/phonetic/parse
         pin1yin1/pin1yin1/parse)

(define (pin1yin1-string->pin1yin1/rest #:interpret-e^-as-e-circumflex? [interpret-e^-as-e-circumflex? #t]
                                        #:interpret-v-as-u-umlaut? [interpret-v-as-u-umlaut? #t]
                                        #:implicit-neutral-tone? [implicit-neutral-tone? #f]
                                        string)
  (match-let ([(cons pin1yin1 rest)
               (parse-string! (then/p (pin1yin1/p #:compound/p
                                                  (compound/p (polysyllable/p #:implicit-neutral-tone?
                                                                              implicit-neutral-tone?
                                                                              (curry syllable/p
                                                                                     #:interpret-e^-as-e-circumflex?
                                                                                     interpret-e^-as-e-circumflex?
                                                                                     #:interpret-v-as-u-umlaut?
                                                                                     interpret-v-as-u-umlaut?)))
                                                  #:non-phonetic/p non-phonetic/p)
                                      (map/p list->string
                                             ->list/p))
                              string)])
    (values pin1yin1 rest)))

(define (pin1yin1-string->pin1yin1 #:interpret-e^-as-e-circumflex? [interpret-e^-as-e-circumflex? #t]
                                   #:interpret-v-as-u-umlaut? [interpret-v-as-u-umlaut? #t]
                                   #:implicit-neutral-tone? [implicit-neutral-tone? #f]
                                   string)
  (let-values ([(pin1yin1 rest)
                (pin1yin1-string->pin1yin1/rest #:interpret-e^-as-e-circumflex? interpret-e^-as-e-circumflex?
                                                #:interpret-v-as-u-umlaut? interpret-v-as-u-umlaut?
                                                #:implicit-neutral-tone? implicit-neutral-tone?
                                                string)])
    (if (equal? "" rest)
        pin1yin1
        #f)))
