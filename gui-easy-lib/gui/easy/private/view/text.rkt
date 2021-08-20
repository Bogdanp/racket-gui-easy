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
    (init-field @label @color font)
    (super-new)

    (define/public (dependencies)
      (filter obs? (list @label @color)))

    (define/public (create parent)
      (define color (peek @color))
      (define the-message
        (new gui:message%
             [parent parent]
             [label (peek @label)]
             [font font]
             [auto-resize #t]))
      (begin0 the-message
        (when color
          (send the-message set-color color))))

    (define/public (update v what val)
      (case/dep what
        [@label (send v set-label val)]
        [@color (send v set-color val)]))

    (define/public (destroy _v)
      (void))))

(define (text @label
              #:color [@color #f]
              #:font [font gui:normal-control-font])
  (new text%
       [@label @label]
       [@color @color]
       [font font]))
