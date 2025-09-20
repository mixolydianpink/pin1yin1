#lang racket/base

(provide (struct-out some)
         (struct-out none))

(require racket/match)

(struct some (value) #:prefab)
(struct none () #:prefab)
