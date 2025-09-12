#lang racket/base

(provide sequence->pst

         (rename-out [pst-root pst?]

                     [pst-root-ref/prefix? pst-ref/prefix?]
                     [pst-root-ref pst-ref]
                     [pst-root-prefix? pst-prefix?]

                     [pst-root->list pst->list]))

(require (only-in racket/list
                  append*
                  empty)
         racket/match
         (only-in racket/stream
                  empty-stream
                  stream-cons)

         pin1yin1/option)

(struct pst-root (comparator head) #:transparent)
(struct pst-head ([value #:mutable] [first-node #:mutable]) #:transparent)
(struct pst-node (key [child-head #:mutable] [next-sibling #:mutable]) #:transparent)

(define (make-pst-leaf-head) (pst-head (none) null))
(define (make-pst-leaf-node key) (pst-node key null null))

(struct found (node predecessor) #:prefab)
(struct rightmost (head-or-node) #:prefab)

(define (pst-head-find-node-and-predecessor-or-rightmost head comparator search-key)
  (define (c=? a b) (eq? 'eq (comparator a b)))
  (define (c<? a b) (eq? 'lt (comparator a b)))
  (let loop ([node-or-null (pst-head-first-node head)]
             [previous head])
    (match node-or-null
      [(and node (pst-node key _ next-sibling))
       (cond
         [(c<? search-key key)
          (rightmost previous)]
         [(c=? search-key key)
          (found node previous)]
         [else
          (loop next-sibling node)])]
      [null
       (rightmost previous)])))

(define (sequence->pst comparator sequence-of-key-list-and-value)
  (let ([root (pst-root comparator (make-pst-leaf-head))])
    (pst-root-set-sequence! root sequence-of-key-list-and-value)
    root))

(define (pst-root-ref/prefix? root key-list)
  (define (pst-head-ref/prefix? head comparator key-list)
    (match key-list
      [(cons next-key rest)
       (match (pst-head-find-node-and-predecessor-or-rightmost head comparator next-key)
         [(found next-node _)
          (pst-head-ref/prefix? (pst-node-child-head next-node) comparator rest)]
         [(rightmost _)
          (values (none) #f)])]
      [empty
       (values (pst-head-value head)
               (not (null? (pst-head-first-node head))))]))
  (pst-head-ref/prefix? (pst-root-head root) (pst-root-comparator root) key-list))

(define (pst-root-ref root key-list)
  (let-values ([(value _) (pst-root-ref/prefix? root key-list)])
    value))

(define (pst-root-prefix? root key-list)
  (let-values ([(_ prefix?) (pst-root-ref/prefix? root key-list)])
    prefix?))

(define (pst-root-set! root key-list value)
  (define (pst-head-set! head comparator key-list value)
    (define (pst-head-find-node-or-insert! head comparator search-key)
      (define (pst-rightmost head-or-node)
        (match head-or-node
          [(and head (pst-head _ first))
           (if (null? first)
               head
               (pst-rightmost first))]
          [(and node (pst-node _ _ next))
           (if (null? next)
               node
               (pst-rightmost next))]))
      (define (insert-right! head-or-node node-to-insert)
        (let ([after-node
               (match head-or-node
                 [(? pst-head? head)
                  (pst-head-first-node head)]
                 [(? pst-node? node)
                  (pst-node-next-sibling node)])])
          (match head-or-node
            [(? pst-head? head)
             (set-pst-head-first-node! head node-to-insert)]
            [(? pst-node? node)
             (set-pst-node-next-sibling! node node-to-insert)])
          (set-pst-node-next-sibling! (pst-rightmost node-to-insert) after-node)))
      (match (pst-head-find-node-and-predecessor-or-rightmost head comparator search-key)
        [(found target-node _)
         target-node]
        [(rightmost rightmost-head-or-node)
         (let ([new-node (make-pst-leaf-node search-key)])
           (insert-right! rightmost-head-or-node new-node)
           new-node)]))
    (define (pst-node-child-head! node)
      (let ([child (pst-node-child-head node)])
        (if (null? child)
            (let ([new-child (make-pst-leaf-head)])
              (set-pst-node-child-head! node new-child)
              new-child)
            child)))
    (match key-list
      [(cons next-key rest)
       (let ([next-node (pst-head-find-node-or-insert! head comparator next-key)])
         (pst-head-set! (pst-node-child-head! next-node) comparator rest value))]
      [empty
       (set-pst-head-value! head (some value))]))
  (pst-head-set! (pst-root-head root) (pst-root-comparator root) key-list value))

(define (pst-root-set-sequence! root sequence-of-key-list-and-value)
  (for ([kv sequence-of-key-list-and-value])
    (match-let ([(cons key-list value) kv])
      (pst-root-set! root key-list value))))

(define (pst-root->list #:prefix [prefix empty] root)
  (define (pst-head->list #:prefix [prefix empty] head)
    (define (pst-head-nodes head)
      (define (pst-node-and-successors node-or-null)
        (match node-or-null
          [(and node (pst-node _ _ next-sibling))
           (stream-cons node (pst-node-and-successors next-sibling))]
          [null
           empty-stream]))
      (pst-node-and-successors (pst-head-first-node head)))
    (let loop ([head-or-null head]
               [reverse-key-list (reverse prefix)])
      (match head-or-null
        [(and head (pst-head value _))
         (append* (match value
                    [(some value)
                     (list (cons (reverse reverse-key-list) value))]
                    [(none) empty])
                  (for/list ([node (pst-head-nodes head)])
                    (match-let ([(pst-node key child _) node])
                      (loop child (cons key reverse-key-list)))))]
        [null
         empty])))
  (define (pst-head-subhead head comparator key-list)
    (match key-list
      [(cons next-key rest)
       (match (pst-head-find-node-and-predecessor-or-rightmost head comparator next-key)
         [(found (pst-node _ next-head _) _)
          (pst-head-subhead next-head comparator rest)]
         [(rightmost _)
          (none)])]
      [empty
       (some head)]))
  (match (pst-head-subhead (pst-root-head root) (pst-root-comparator root) prefix)
    [(some subhead)
     (pst-head->list #:prefix prefix subhead)]
    [(none)
     empty]))
