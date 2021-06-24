#lang racket/base

(require racket/class
         (prefix-in gui: racket/gui)
         racket/match
         "../observable.rkt"
         "common.rkt"
         "view.rkt")

(provide
 slider)

(define slider%
  (class* object% (view<%>)
    (init-field @label @enabled? @value @min-size @stretch min-value max-value style action)
    (super-new)

    (define/public (dependencies)
      (filter obs? (list @label @enabled? @value @min-size @stretch)))

    (define/public (create parent)
      (match-define (list min-w min-h) (peek @min-size))
      (match-define (list w-s? h-s?) (peek @stretch))
      (new gui:slider%
           [parent parent]
           [label (peek @label)]
           [style style]
           [init-value (peek @value)]
           [min-value min-value]
           [max-value max-value]
           [enabled (peek @enabled?)]
           [callback (λ (self _event)
                       (action (send self get-value)))]
           [min-width min-w]
           [min-height min-h]
           [stretchable-width w-s?]
           [stretchable-height h-s?]))

    (define/public (update v what val)
      (case/dep what
        [@label (send v set-label val)]
        [@enabled? (send v enable val)]
        [@value (send v set-value val)]
        [@min-size
         (match-define (list w h) val)
         (send* v
           (min-width w)
           (min-height h))]
        [@stretch
         (match-define (list w-s? h-s?) val)
         (send* v
           (stretchable-width w-s?)
           (stretchable-height h-s?))]))

    (define/public (destroy _v)
      (void))))

(define (slider @value action
                #:label [@label #f]
                #:style [style '(horizontal)]
                #:enabled? [@enabled? #t]
                #:min-value [min-value 0]
                #:max-value [max-value 100]
                #:min-size [@min-size '(#f #f)]
                #:stretch [@stretch (list (memq 'horizontal style)
                                          (memq 'vertical style))])
  (new slider%
       [@value @value]
       [@label @label]
       [style style]
       [@enabled? @enabled?]
       [min-value min-value]
       [max-value max-value]
       [@min-size @min-size]
       [@stretch @stretch]
       [action action]))
