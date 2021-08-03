#lang racket/base

(require racket/class)

(provide
 container%)

(define container%
  (class object%
    (init-field children)
    (super-new)

    (define deps-to-children
      (for*/fold ([h (hash)])
                 ([c (in-list children)]
                  [d (in-list (send c dependencies))])
        (hash-update h d (λ (cs) (cons c cs)) null)))

    (define/public (child-dependencies)
      (hash-keys deps-to-children))

    (define/public (update-children what val)
      (for ([c (in-list (hash-ref deps-to-children what null))])
        (send c update (get-child c) what val)))

    (define/public (destroy-children)
      (for ([(c w) (in-hash children-to-widgets)])
        (send c destroy w)
        (remove-child c)))

    (define children-to-widgets (make-hasheq))

    (define/public (add-child c w)
      (hash-set! children-to-widgets c w))

    (define/public (get-child c)
      (hash-ref children-to-widgets c))

    (define/public (has-child? c)
      (hash-has-key? children-to-widgets c))

    (define/public (remove-child c)
      (hash-remove! children-to-widgets c))))
