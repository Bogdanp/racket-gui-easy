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

(struct props (image size mode))

(define image%
  (class* object% (view<%>)
    (init-field @image @size @mode)
    (super-new)

    (define @props
      (obs-combine props @image @size @mode))

    (define/public (dependencies)
      (filter obs? (list @props)))

    (define/public (create parent)
      (match-define (props image size mode)
        (peek @props))
      (define bmp (read-bitmap image))
      (define bmp/scaled (scale bmp size mode))
      (define the-canvas
        (new (context-mixin gui:canvas%)
             [parent parent]
             [style '(transparent)]
             [min-width (send bmp/scaled get-width)]
             [min-height (send bmp/scaled get-height)]
             [stretchable-width #f]
             [stretchable-height #f]
             [paint-callback (λ (self dc)
                               (let ([bmp (send self get-context 'bmp/scaled bmp/scaled)])
                                 (send dc draw-bitmap bmp 0 0)))]))
      (begin0 the-canvas
        (send the-canvas set-context* 'image image 'bmp bmp 'bmp/scaled bmp/scaled)))

    (define/public (update v what val)
      (case/dep what
        [@props
         (define last-image
           (send v get-context 'image))
         (match-define (props image size mode) val)
         (define bmp
           (if (equal? image last-image)
               (send v get-context 'bmp)
               (read-bitmap image)))
         (define bmp/scaled
           (scale bmp size mode))
         (send v set-context* 'image image 'bmp bmp 'bmp/scaled bmp/scaled)
         (send v min-width (send bmp/scaled get-width))
         (send v min-height (send bmp/scaled get-height))
         (send v refresh-now)]))

    (define/public (destroy v)
      (send v clear-context))

    (define (read-bitmap image)
      (if (gui:is-a? image gui:bitmap%)
          image
          (gui:read-bitmap
           #:try-@2x? #t
           image)))

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

(define (image @image
               #:size [@size (obs '(#f #f))]
               #:mode [@mode (obs 'fit)])
  (new image%
       [@image (->obs @image)]
       [@size (->obs @size)]
       [@mode (->obs @mode)]))
