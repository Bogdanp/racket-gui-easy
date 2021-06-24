#lang racket/base

(require racket/class
         (prefix-in gui: racket/gui)
         "../observable.rkt"
         "common.rkt"
         "view.rkt")

(provide
 text)

(define text%
  (class* object% (view<%>)
    (init-field @label)
    (super-new)

    (define/public (dependencies)
      (filter obs? (list @label)))

    (define/public (create parent)
      (new gui:message%
           [parent parent]
           [label (peek @label)]
           [auto-resize #t]))

    (define/public (update v what val)
      (case/dep what
        [@label (send v set-label val)]))

    (define/public (destroy _v)
      (void))))

(define (text @label)
  (new text%
       [@label @label]))
