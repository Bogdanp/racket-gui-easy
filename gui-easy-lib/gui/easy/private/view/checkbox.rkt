#lang racket/base

(require racket/class
         (prefix-in gui: racket/gui)
         "../observable.rkt"
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
           [callback (λ (_self _event)
                       (action))]))

    (define/public (update v what val)
      (cond
        [(eq? what @label) (send v set-label val)]
        [(eq? what @checked?) (send v set-value val)]
        [(eq? what @enabled?) (send v enable val)]
        [else (void)]))

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
