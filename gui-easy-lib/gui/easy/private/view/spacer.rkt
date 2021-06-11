#lang racket/base

(require racket/class
         (prefix-in gui: racket/gui)
         "../observable.rkt"
         "view.rkt")

(provide
 spacer)

(define spacer%
  (class* object% (view<%>)
    (super-new)

    (define/public (dependencies)
      null)

    (define/public (create parent)
      (new gui:panel%
           [parent parent]))

    (define/public (update v _what val)
      (void))

    (define/public (destroy _v)
      (void))))

(define (spacer)
  (new spacer%))
