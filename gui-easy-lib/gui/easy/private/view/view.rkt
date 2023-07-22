#lang racket/base

(require racket/class)

(provide
 view<%>
 context<%>
 context-mixin)

(define view<%>
  (interface () dependencies create update destroy))

(define context<%>
  (interface ()
    set-context
    set-context*
    get-context
    get-context!
    remove-context
    clear-context))

(define (context-mixin %)
  (class* % (context<%>)
    (field [ctx (make-hasheq)])
    (super-new)

    (define/public (set-context k v)
      (hash-set! ctx k v))
    (define/public (set-context* . k&vs)
      (apply hash-set*! ctx k&vs))
    (define/public (get-context k [default (λ () (error 'get-context "no entry for ~a" k))])
      (hash-ref ctx k default))
    (define/public (get-context! k to-set)
      (hash-ref! ctx k to-set))
    (define/public (remove-context k)
      (hash-remove! ctx k))
    (define/public (clear-context)
      (hash-clear! ctx))))
