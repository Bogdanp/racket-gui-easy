#lang racket/base

(require racket/class
         (prefix-in gui: racket/gui)
         racket/match
         "../observable.rkt"
         "common.rkt"
         "view.rkt")

(provide
 progress)

(define progress%
  (class* object% (view<%>)
    (init-field @label @enabled? @range @value @min-size @stretch style)
    (super-new)

    (define/public (dependencies)
      (list @label @enabled? @range @value @min-size @stretch))

    (define/public (create parent)
      (match-define (list min-w min-h) (obs-peek @min-size))
      (match-define (list w-s? h-s?) (obs-peek @stretch))
      (define the-gauge
        (new gui:gauge%
             [parent parent]
             [label (obs-peek @label)]
             [style style]
             [range (obs-peek @range)]
             [enabled (obs-peek @enabled?)]
             [min-width min-w]
             [min-height min-h]
             [stretchable-width w-s?]
             [stretchable-height h-s?]))
      (begin0 the-gauge
        (send the-gauge set-value (obs-peek @value))))

    (define/public (update v what val)
      (case/dep what
        [@label (send v set-label val)]
        [@enabled? (send v enable val)]
        [@range (send v set-range val)]
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

(define (progress @value
                  #:label [@label (obs #f)]
                  #:style [style '(horizontal)]
                  #:enabled? [@enabled? (obs #t)]
                  #:range [@range (obs 100)]
                  #:min-size [@min-size (obs '(#f #f))]
                  #:stretch [@stretch (obs (list (memq 'horizontal style)
                                                 (memq 'vertical style)))])
  (new progress%
       [@value (->obs @value)]
       [@label (->obs @label)]
       [style style]
       [@enabled? (->obs @enabled?)]
       [@range (->obs @range)]
       [@min-size (->obs @min-size)]
       [@stretch (->obs @stretch)]))
