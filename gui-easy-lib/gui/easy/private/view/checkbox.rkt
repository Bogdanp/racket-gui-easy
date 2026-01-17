#lang racket/base

(require racket/class
         (prefix-in gui: racket/gui)
         "../observable.rkt"
         "common.rkt"
         "view.rkt")

(provide
 checkbox)

(define (make-checkbox% gui-check-box%)
  (class* object% (view<%>)
    (init-field @label @checked? @enabled? action)
    (super-new)

    (define/public (dependencies)
      (filter obs? (list @label @checked? @enabled?)))

    (define/public (create parent)
      (new gui-check-box%
           [parent parent]
           [label (peek @label)]
           [value (peek @checked?)]
           [enabled (peek @enabled?)]
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
                  #:label [@label ""]
                  #:checked? [@checked? #f]
                  #:enabled? [@enabled? #t]
                  #:mixin [mix values])
  (new (make-checkbox% (mix gui:check-box%))
       [@label @label]
       [@checked? @checked?]
       [@enabled? @enabled?]
       [action action]))
