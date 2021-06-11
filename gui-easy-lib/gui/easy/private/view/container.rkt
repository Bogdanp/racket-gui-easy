#lang racket/base

(require racket/class)

(provide
 container%)

(define container%
  (class object%
    (init-field children)
    (super-new)

    (define deps-to-children
      (for*/fold ([h (hasheq)])
                 ([c (in-list children)]
                  [d (in-list (send c dependencies))])
        (hash-update h d (λ (cs) (cons c cs)) null)))

    (define/public (child-dependencies)
      (hash-keys deps-to-children))

    (define/public (children-for-dep dep)
      (hash-ref deps-to-children dep null))

    (define children-to-widgets (make-hasheq))

    (define/public (get-children)
      (for/hasheq ([(k v) (in-hash children-to-widgets)])
        (values k v)))

    (define/public (add-child c w)
      (hash-set! children-to-widgets c w))

    (define/public (get-child c)
      (hash-ref children-to-widgets c))

    (define/public (has-child? c)
      (hash-has-key? children-to-widgets c))

    (define/public (remove-child c)
      (hash-remove! children-to-widgets c))))
