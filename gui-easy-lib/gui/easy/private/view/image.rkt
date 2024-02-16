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
        (send the-canvas set-context*
              'image image
              'size size
              'mode mode
              'bmp bmp
              'bmp/scaled bmp/scaled)))

    (define/public (update v what val)
      (case/dep what
        [@props
         (define last-image (send v get-context 'image))
         (define last-size (send v get-context 'size))
         (define last-mode (send v get-context 'mode))
         (define last-bmp (send v get-context 'bmp))
         (define last-bmp/scaled (send v get-context 'bmp/scaled))
         (match-define (props image size mode) val)
         (define bmp
           (if (equal? image last-image)
               (send v get-context 'bmp)
               (read-bitmap image)))
         (define bmp/scaled
           (if (and (equal? bmp last-bmp)
                    (equal? size last-size)
                    (equal? mode last-mode))
               last-bmp/scaled
               (scale bmp size mode)))
         (send v set-context*
               'image image
               'size size
               'mode mode
               'bmp bmp
               'bmp/scaled bmp/scaled)
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
         (define-values (src-w src-h)
           (values
            (send bmp get-width)
            (send bmp get-height)))
         (define aspect-ratio (/ src-h src-w))
         (define-values (pw ph)
           (values
            (or w (exact-ceiling (/ h aspect-ratio)))
            (or h (exact-ceiling (* w aspect-ratio)))))
         (define-values (dst-w dst-h)
           (case mode
             [(fill)
              (values pw ph)]

             [(fit)
              (if (>= (* pw aspect-ratio) ph)
                  (values (exact-ceiling (/ ph aspect-ratio)) ph)
                  (values pw (exact-ceiling (* pw aspect-ratio))))]))
         (define bmp-dc
           (new gui:bitmap-dc%
                [bitmap (gui:make-bitmap dst-w dst-h #:backing-scale backing-scale)]))
         (send bmp-dc draw-bitmap-section-smooth
               bmp
               0 0 dst-w dst-h
               0 0 src-w src-h)
         (send bmp-dc get-bitmap)]))))

(define (image @image
               #:size [@size (obs '(#f #f))]
               #:mode [@mode (obs 'fit)])
  (new image%
       [@image (->obs @image)]
       [@size (->obs @size)]
       [@mode (->obs @mode)]))
