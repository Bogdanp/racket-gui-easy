#lang racket/base

(require racket/class
         "view.rkt")

(provide
 add-hooks)

(define proxy%
  (class* object% (view<%>)
    (init-field on-create-proc
                on-destroy-proc
                the-view)
    (super-new)
    (define/public (dependencies)
      (send the-view dependencies))
    (define/public (create parent)
      (begin0 (send the-view create parent)
        (on-create-proc)))
    (define/public (update v what val)
      (send the-view update v what val))
    (define/public (destroy v)
      (begin0 (send the-view destroy v)
        (on-destroy-proc)))))

(define (add-hooks #:on-create [on-create-proc void]
                   #:on-destroy [on-destroy-proc void]
                   the-view)
  (new proxy%
       [on-create-proc on-create-proc]
       [on-destroy-proc on-destroy-proc]
       [the-view the-view]))
