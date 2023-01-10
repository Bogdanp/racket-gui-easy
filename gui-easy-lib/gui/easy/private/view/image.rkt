#lang racket/base

(require racket/class
         (prefix-in gui: racket/gui)
         racket/match
         racket/math
         "../observable.rkt"
         "common.rkt"
         "view.rkt")

(provide
 image)

;; TODO: This should change when the display changes.
(define backing-scale
  (gui:get-display-backing-scale))

(define image%
  (class* object% (view<%>)
    (init-field @path @size @mode)
    (super-new)

    (define @path+size+mode
      (obs-combine list @path @size @mode))

    (define/public (dependencies)
      (filter obs? (list @path+size+mode)))

    (define/public (create parent)
      (match-define (list path size mode)
        (peek @path+size+mode))
      (define bmp (gui:read-bitmap path))
      (define bmp/scaled (scale bmp size mode))
      (define the-canvas
        (new (context-mixin gui:canvas%)
             [parent parent]
             [min-width (send bmp/scaled get-width)]
             [min-height (send bmp/scaled get-height)]
             [stretchable-width #f]
             [stretchable-height #f]
             [paint-callback (λ (self dc)
                               (define bmp
                                 (send self get-context 'bmp/scaled))
                               (when bmp
                                 (send dc draw-bitmap bmp 0 0)))]))
      (begin0 the-canvas
        (send the-canvas set-context* 'path path 'bmp bmp 'bmp/scaled bmp/scaled)))

    (define/public (update v what val)
      (case/dep what
        [@path+size+mode
         (define last-path
           (send v get-context 'path))
         (match-define (list path size mode) val)
         (define bmp
           (if (equal? path last-path)
               (send v get-context 'bmp)
               (gui:read-bitmap path)))
         (define bmp/scaled
           (scale bmp size mode))
         (send v set-context* 'path path 'bmp bmp 'bmp/scaled bmp/scaled)
         (send v min-width (send bmp/scaled get-width))
         (send v min-height (send bmp/scaled get-height))
         (send v refresh-now)]))

    (define/public (destroy v)
      (send v clear-context))

    (define (scale bmp size mode)
      (match size
        ['(#f #f) bmp]
        [`(,w ,h)
         (define-values (sw sh)
           (values
            (send bmp get-width)
            (send bmp get-height)))
         (define-values (w* h*)
           (case mode
             [(fill)
              (values w h)]

             [(fit)
              (define r (/ sh sw))
              (if (>= (* w r) h)
                  (values (exact-ceiling (/ h r)) h)
                  (values w (exact-ceiling (* w r))))]))
         (define bmp-dc
           (new gui:bitmap-dc%
                [bitmap (gui:make-bitmap w* h* #:backing-scale backing-scale)]))
         (send bmp-dc draw-bitmap-section-smooth
               bmp
               0 0 w* h*
               0 0 sw sh)
         (send bmp-dc get-bitmap)]))))

(define (image @path
               #:size [@size (obs '(#f #f))]
               #:mode [@mode (obs 'fit)])
  (new image%
       [@path (->obs @path)]
       [@size (->obs @size)]
       [@mode (->obs @mode)]))
