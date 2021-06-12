#lang racket/base

(require racket/class
         (prefix-in gui: racket/gui)
         "../observable.rkt"
         "view.rkt")

(provide
 text)

(define text%
  (class* object% (view<%>)
    (init-field @label)
    (super-new)

    (define/public (dependencies)
      (list @label))

    (define/public (create parent)
      (new gui:message%
           [parent parent]
           [label (obs-peek @label)]
           [auto-resize #t]))

    (define/public (update v _what val)
      (send v set-label val))

    (define/public (destroy _v)
      (void))))

(define (text @label)
  (new text%
       [@label (->obs @label)]))
