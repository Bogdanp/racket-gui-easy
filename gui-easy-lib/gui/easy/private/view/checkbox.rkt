#lang racket/base

(require racket/class
         (prefix-in gui: racket/gui)
         "../observable.rkt"
         "common.rkt"
         "view.rkt")

(provide
 checkbox)

(define checkbox%
  (class* object% (view<%>)
    (init-field @label @checked? @enabled? action)
    (super-new)

    (define/public (dependencies)
      (list @label @checked? @enabled?))

    (define/public (create parent)
      (new gui:check-box%
           [parent parent]
           [label (obs-peek @label)]
           [value (obs-peek @checked?)]
           [enabled (obs-peek @enabled?)]
           [callback (λ (self _event)
                       (action (send self get-value)))]))

    (define/public (update v what val)
      (case/dep what
        [@label (send v set-label val)]
        [@checked? (send v set-value val)]
        [@enabled? (send v enable val)]))

    (define/public (destroy _v)
      (void))))

(define (checkbox action
                  #:label [@label (obs #f)]
                  #:checked? [@checked? (obs #f)]
                  #:enabled? [@enabled? (obs #t)])
  (new checkbox%
       [@label (->obs @label)]
       [@checked? (->obs @checked?)]
       [@enabled? (->obs @enabled?)]
       [action action]))
