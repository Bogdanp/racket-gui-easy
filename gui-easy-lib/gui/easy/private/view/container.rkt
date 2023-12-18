#lang racket/base

(require racket/class)

(provide
 container%
 with-container-sequence)

(define container%
  (class object%
    (init-field children)
    (super-new)

    (define deps-to-children (make-hash))
    (for* ([c (in-list children)]
           [d (in-list (send c dependencies))])
      (hash-update! deps-to-children d (λ (cs) (cons c cs)) null))

    (define/public (child-dependencies)
      (hash-keys deps-to-children))

    (define/public (update-children v what val)
      (for ([c (in-list (hash-ref deps-to-children what null))])
        (send c update (get-child v c) what val)))

    (define/public (destroy-children v)
      (for ([(c w) (in-hash (get-children-to-widgets v))])
        (send c destroy w)
        (remove-child v c))
      (hash-clear! (get-children-to-widgets v)))

    (define/public (add-child v c w)
      (hash-set! (get-children-to-widgets v) c w))

    (define/public (get-child v c [default (lambda () (error 'get-child "child not found: ~e" c))])
      (hash-ref (get-children-to-widgets v) c default))

    (define/public (has-child? v c)
      (hash-has-key? (get-children-to-widgets v) c))

    (define/public (remove-child v c)
      (hash-remove! (get-children-to-widgets v) c))

    (define/private (get-children-to-widgets v)
      (send v get-context! 'children-to-widgets make-hasheq))))

(define-syntax-rule (with-container-sequence container body0 body ...)
  (dynamic-wind
    (lambda ()
      (send container begin-container-sequence))
    (lambda ()
      body0
      body ...)
    (lambda ()
      (send container end-container-sequence))))
