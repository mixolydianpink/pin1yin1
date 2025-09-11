#lang racket/base

(provide pin1yin1->pinyin/html-fragment
         pin1yin1->zhuyin/html-fragment)

(require (only-in racket/function
                  curry)

         pin1yin1/phonetic/markup
         pin1yin1/pin1yin1)

(define (pin1yin1->html-fragment #:complex->html-fragment complex->html-fragment
                                 #:non-phonetic->html-fragment non-phonetic->html-fragment
                                 pin1yin1)
  (pin1yin1-append-map #:complex->list complex->html-fragment
                       #:non-phonetic->list non-phonetic->html-fragment
                       pin1yin1))

(define (pin1yin1->pinyin/html-fragment #:explicit-neutral-tone? explicit-neutral-tone?
                                        #:syllable-first-tone-class syllable-first-tone-class
                                        #:syllable-second-tone-class syllable-second-tone-class
                                        #:syllable-third-tone-class syllable-third-tone-class
                                        #:syllable-fourth-tone-class syllable-fourth-tone-class
                                        #:syllable-neutral-tone-class syllable-neutral-tone-class
                                        #:non-phonetic->html-fragment non-phonetic->html-fragment
                                        pin1yin1)
  (pin1yin1->html-fragment #:complex->html-fragment
                           (curry complex->pinyin/html-fragment
                                  #:explicit-neutral-tone? explicit-neutral-tone?
                                  #:syllable-first-tone-class syllable-first-tone-class
                                  #:syllable-second-tone-class syllable-second-tone-class
                                  #:syllable-third-tone-class syllable-third-tone-class
                                  #:syllable-fourth-tone-class syllable-fourth-tone-class
                                  #:syllable-neutral-tone-class syllable-neutral-tone-class)
                           #:non-phonetic->html-fragment non-phonetic->html-fragment
                           pin1yin1))

(define (pin1yin1->zhuyin/html-fragment #:explicit-first-tone? explicit-first-tone?
                                        #:prefix-neutral-tone? prefix-neutral-tone?
                                        #:explicit-empty-rhyme? explicit-empty-rhyme?
                                        #:syllabic-m? syllabic-m?
                                        #:syllabic-n? syllabic-n?
                                        #:syllabic-ng? syllabic-ng?
                                        #:syllable-first-tone-class syllable-first-tone-class
                                        #:syllable-second-tone-class syllable-second-tone-class
                                        #:syllable-third-tone-class syllable-third-tone-class
                                        #:syllable-fourth-tone-class syllable-fourth-tone-class
                                        #:syllable-neutral-tone-class syllable-neutral-tone-class
                                        #:non-phonetic->html-fragment non-phonetic->html-fragment
                                        pin1yin1)
  (pin1yin1->html-fragment #:complex->html-fragment
                           (curry complex->zhuyin/html-fragment
                                  #:explicit-first-tone? explicit-first-tone?
                                  #:prefix-neutral-tone? prefix-neutral-tone?
                                  #:explicit-empty-rhyme? explicit-empty-rhyme?
                                  #:syllabic-m? syllabic-m?
                                  #:syllabic-n? syllabic-n?
                                  #:syllabic-ng? syllabic-ng?
                                  #:syllable-first-tone-class syllable-first-tone-class
                                  #:syllable-second-tone-class syllable-second-tone-class
                                  #:syllable-third-tone-class syllable-third-tone-class
                                  #:syllable-fourth-tone-class syllable-fourth-tone-class
                                  #:syllable-neutral-tone-class syllable-neutral-tone-class)
                           #:non-phonetic->html-fragment non-phonetic->html-fragment
                           pin1yin1))
