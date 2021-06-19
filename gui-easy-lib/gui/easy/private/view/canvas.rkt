#lang racket/base

(require racket/class
         (prefix-in gui: racket/gui)
         racket/match
         "../observable.rkt"
         "common.rkt"
         "view.rkt")

(provide
 canvas)

(define canvas%
  (class* object% (view<%>)
    (init-field @input @label @enabled? @margin @min-size @stretch draw style)
    (super-new)

    (define input #f)

    (define/public (dependencies)
      (list @input @label @enabled? @margin @min-size @stretch))

    (define/public (create parent)
      (match-define (list h-m v-m) (obs-peek @margin))
      (match-define (list min-w min-h) (obs-peek @min-size))
      (match-define (list w-s? h-s?) (obs-peek @stretch))
      (set! input (obs-peek @input))
      (new gui:canvas%
           [parent parent]
           [paint-callback (λ (_self dc)
                             (draw dc input))]
           [label (obs-peek @label)]
           [enabled (obs-peek @enabled?)]
           [style style]
           [horiz-margin h-m]
           [vert-margin v-m]
           [min-width min-w]
           [min-height min-h]
           [stretchable-width w-s?]
           [stretchable-height h-s?]))

    (define/public (update v what val)
      (case/dep what
        [@input
         (set! input val)
         (send v refresh)]
        [@label
         (send v set-label val)]
        [@enabled?
         (send v enable val)]
        [@margin
         (match-define (list h v) val)
         (send* v
           (horiz-margin h)
           (vert-margin v))]
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

(define (canvas @input draw
                #:label [@label (obs #f)]
                #:enabled? [@enabled? (obs #f)]
                #:style [style null]
                #:margin [@margin (obs '(0 0))]
                #:min-size [@min-size (obs '(#f #f))]
                #:stretch [@stretch (obs '(#t #t))])
  (new canvas%
       [@input (->obs @input)]
       [@label (->obs @label)]
       [@enabled? (->obs @enabled?)]
       [@margin (->obs @margin)]
       [@min-size (->obs @min-size)]
       [@stretch (->obs @stretch)]
       [draw draw]
       [style style]))
