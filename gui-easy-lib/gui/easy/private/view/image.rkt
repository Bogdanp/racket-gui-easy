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

    (define bmp-path #f)
    (define bmp #f)
    (define bmp/scaled #f)

    (define @path+size+mode
      (obs-combine list @path @size @mode))

    (define/public (dependencies)
      (filter obs? (list @path+size+mode)))

    (define/public (create parent)
      (apply load! (peek @path+size+mode))
      (define-values (w h)
        (get-effective-size))
      (new gui:canvas%
           [parent parent]
           [min-width w]
           [min-height h]
           [stretchable-width #f]
           [stretchable-height #f]
           [paint-callback (λ (_self dc)
                             (send dc draw-bitmap bmp/scaled 0 0))]))

    (define/public (update v what val)
      (case/dep what
        [@path+size+mode
         (match-define (list path size mode) val)
         (when path
           (load! path size mode)
           (define-values (w h) (get-effective-size))
           (send v min-width w)
           (send v min-height h)
           (send v refresh-now))]))

    (define/public (destroy _v)
      (void))

    (define/private (get-effective-size)
      (values
       (send bmp/scaled get-width)
       (send bmp/scaled get-height)))

    (define (load! path size mode)
      (unless (equal? bmp-path path)
        (set! bmp (gui:read-bitmap path))
        (set! bmp-path path))
      (match size
        ['(#f #f)
         (set! bmp/scaled bmp)]

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
         (set! bmp/scaled (send bmp-dc get-bitmap))]))))

(define (image @path
               #:size [@size (obs '(#f #f))]
               #:mode [@mode (obs 'fit)])
  (new image%
       [@path (->obs @path)]
       [@size (->obs @size)]
       [@mode (->obs @mode)]))
