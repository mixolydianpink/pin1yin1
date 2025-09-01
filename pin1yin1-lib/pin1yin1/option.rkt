#lang racket/base

(provide (struct-out some)
         (struct-out none)

         option-map)

(require racket/match)

(struct some (value) #:prefab)
(struct none () #:prefab)

(define (option-map f option)
  (match option
    [(some value)
     (some (f value))]
    [(none)
     (none)]))
