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
      (filter obs? (list @label @enabled? @range @value @min-size @stretch)))

    (define/public (create parent)
      (match-define (list min-w min-h) (peek @min-size))
      (match-define (list w-s? h-s?) (peek @stretch))
      (define the-gauge
        (new gui:gauge%
             [parent parent]
             [label (peek @label)]
             [style style]
             [range (peek @range)]
             [enabled (peek @enabled?)]
             [min-width min-w]
             [min-height min-h]
             [stretchable-width w-s?]
             [stretchable-height h-s?]))
      (begin0 the-gauge
        (send the-gauge set-value (peek @value))))

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
                  #:label [@label #f]
                  #:style [style '(horizontal)]
                  #:enabled? [@enabled? #t]
                  #:range [@range 100]
                  #:min-size [@min-size '(#f #f)]
                  #:stretch [@stretch (list (memq 'horizontal style)
                                            (memq 'vertical style))])
  (new progress%
       [@value @value]
       [@label @label]
       [style style]
       [@enabled? @enabled?]
       [@range @range]
       [@min-size @min-size]
       [@stretch @stretch]))
