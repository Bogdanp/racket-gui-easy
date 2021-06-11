#lang racket/base

(require racket/class)

(provide
 container%)

(define container%
  (class object%
    (init-field children)
    (super-new)

    (define obs-to-children
      (for*/fold ([h (hasheq)])
                 ([c (in-list children)]
                  [d (in-list (send c dependencies))])
        (hash-update h d (λ (cs) (cons c cs)) null)))

    (define/public (unique-obs)
      (hash-keys obs-to-children))

    (define/public (children-for-obs o)
      (hash-ref obs-to-children o null))

    (define child-frames (make-hasheq))

    (define/public (get-child-frames)
      (for/hasheq ([(k v) (in-hash child-frames)])
        (values k v)))

    (define/public (add-child-frame c f)
      (hash-set! child-frames c f))

    (define/public (get-child-frame c)
      (hash-ref child-frames c))

    (define/public (has-child-frame? c)
      (hash-has-key? child-frames c))

    (define/public (remove-child-frame c)
      (hash-remove! child-frames c))))
