#lang racket/base

(require racket/class)

(provide
 view<%>
 context-mixin)

(define view<%>
  (interface () dependencies create update destroy))

(define (context-mixin %)
  (class %
    (super-new)

    (define ctx (make-hasheq))
    (define/public (set-context k v)
      (hash-set! ctx k v))
    (define/public (set-context* . k&vs)
      (apply hash-set*! ctx k&vs))
    (define/public (get-context k [default (λ () (error 'get-context "no entry for ~a" k))])
      (hash-ref ctx k default))
    (define/public (remove-context k)
      (hash-remove! ctx k))
    (define/public (clear-context)
      (hash-clear! ctx))))
