#lang racket/base

(require (prefix-in p: pict)
         racket/class
         (prefix-in gui: racket/gui)
         racket/match
         "../observable.rkt"
         "common.rkt"
         "view.rkt")

(provide
 canvas
 pict-canvas)

(define canvas%
  (class* object% (view<%>)
    (init-field @input @label @enabled? @margin @min-size @stretch draw style mouse-action)
    (super-new)

    (define input #f)

    (define/public (dependencies)
      (filter obs? (list @input @label @enabled? @margin @min-size @stretch)))

    (define/public (create parent)
      (match-define (list h-m v-m) (peek @margin))
      (match-define (list min-w min-h) (peek @min-size))
      (match-define (list w-s? h-s?) (peek @stretch))
      (set! input (peek @input))
      (new (class gui:canvas%
             (super-new)
             (define/override (on-event e)
               (mouse-action e)))
           [parent parent]
           [paint-callback (λ (_self dc)
                             (draw dc input))]
           [label (peek @label)]
           [enabled (peek @enabled?)]
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
                #:label [@label #f]
                #:enabled? [@enabled? #t]
                #:style [style null]
                #:margin [@margin '(0 0)]
                #:min-size [@min-size '(#f #f)]
                #:stretch [@stretch '(#t #t)]
                #:mouse-action [mouse-action void])
  (new canvas%
       [@input @input]
       [@label @label]
       [@enabled? @enabled?]
       [@margin @margin]
       [@min-size @min-size]
       [@stretch @stretch]
       [draw draw]
       [style style]
       [mouse-action mouse-action]))

(define pict-canvas
  (procedure-rename
   (make-keyword-procedure
    (λ (kws kw-args @data make-pict . args)
      (define (draw dc v)
        (send dc set-smoothing 'smoothed)
        (p:draw-pict (make-pict v) dc 0 0))
      (keyword-apply canvas kws kw-args @data draw args)))
   'pict-canvas))
