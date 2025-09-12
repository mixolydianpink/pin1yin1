#lang racket/base

(provide pin1yin1-string->pin1yin1/rest
         pin1yin1-string->pin1yin1)

(require racket/match

         pin1yin1/parse
         pin1yin1/pin1yin1/parse)

(define (pin1yin1-string->pin1yin1/rest #:interpret-v-as-u-umlaut? [interpret-v-as-u-umlaut? #t]
                                        #:interpret-e^-as-e-circumflex? [interpret-e^-as-e-circumflex? #t]
                                        #:implicit-neutral-tone? [implicit-neutral-tone? #f]
                                        str)
  (match-let ([(cons pin1yin1 rest)
               (parse-string! (then/p (make-pin1yin1/p #:interpret-v-as-u-umlaut? interpret-v-as-u-umlaut?
                                                       #:interpret-e^-as-e-circumflex? interpret-e^-as-e-circumflex?
                                                       #:implicit-neutral-tone? implicit-neutral-tone?)
                                      (map/p list->string
                                             ->list/p))
                              str)])
    (values pin1yin1 rest)))

(define (pin1yin1-string->pin1yin1 #:interpret-v-as-u-umlaut? [interpret-v-as-u-umlaut? #t]
                                   #:interpret-e^-as-e-circumflex? [interpret-e^-as-e-circumflex? #t]
                                   #:implicit-neutral-tone? [implicit-neutral-tone? #f]
                                   str)
  (let-values ([(pin1yin1 rest)
                (pin1yin1-string->pin1yin1/rest #:interpret-v-as-u-umlaut? interpret-v-as-u-umlaut?
                                                #:interpret-e^-as-e-circumflex? interpret-e^-as-e-circumflex?
                                                #:implicit-neutral-tone? implicit-neutral-tone?
                                                str)])
    (if (equal? "" rest)
        pin1yin1
        #f)))
