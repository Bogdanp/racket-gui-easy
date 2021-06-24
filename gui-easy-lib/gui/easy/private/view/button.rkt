#lang racket/base

(require racket/class
         (prefix-in gui: racket/gui)
         "../observable.rkt"
         "common.rkt"
         "view.rkt")

(provide
 button)

(define button%
  (class* object% (view<%>)
    (init-field @label action)
    (super-new)

    (define/public (dependencies)
      (filter obs? (list @label)))

    (define/public (create parent)
      (new gui:button%
           [parent parent]
           [label (peek @label)]
           [callback (λ (_self _event)
                       (action))]))

    (define/public (update v what val)
      (case/dep what
        [@label (send v set-label val)]))

    (define/public (destroy _v)
      (void))))

(define (button @label action)
  (new button%
       [@label @label]
       [action action]))
