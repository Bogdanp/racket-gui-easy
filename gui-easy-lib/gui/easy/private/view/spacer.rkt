#lang racket/base

(require racket/class
         (prefix-in gui: racket/gui)
         "view.rkt")

(provide
 spacer)

(define (make-spacer% gui-panel%)
  (class* object% (view<%>)
    (super-new)

    (define/public (dependencies)
      null)

    (define/public (create parent)
      (new gui-panel%
           [parent parent]))

    (define/public (update _v _what _val)
      (void))

    (define/public (destroy _v)
      (void))))

(define (spacer #:mixin [mix values])
  (new (make-spacer% (mix gui:panel%))))
