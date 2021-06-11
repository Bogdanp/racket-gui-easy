#lang racket/base

(require racket/class
         (prefix-in gui: racket/gui)
         "../observable.rkt"
         "view.rkt")

(provide
 button)

(define button%
  (class* object% (view<%>)
    (init-field @label action)
    (super-new)

    (define/public (dependencies)
      (list @label))

    (define/public (create parent)
      (new gui:button%
           [parent parent]
           [label (obs-peek @label)]
           [callback (λ (_self _event)
                       (action))]))

    (define/public (update v _what val)
      (send v set-label val))

    (define/public (destroy _v)
      (void))))

(define (button @label action)
  (new button%
       [@label (->obs @label)]
       [action action]))
