#lang racket/base

(require racket/class
         (prefix-in gui: racket/gui)
         racket/match
         "../observable.rkt"
         "common.rkt"
         "view.rkt")

(provide
 image)

;; TODO: This should react to changes to the backing scale.
(define scale
  (gui:get-display-backing-scale))

(define image%
  (class* object% (view<%>)
    (init-field @path)
    (super-new)

    (define bmp #f)

    (define/public (dependencies)
      (list @path))

    (define/public (create parent)
      (reload! (obs-peek @path))
      (new gui:canvas%
           [parent parent]
           [min-width (send bmp get-width)]
           [min-height (send bmp get-height)]
           [stretchable-width #f]
           [stretchable-height #f]
           [paint-callback (λ (_self dc)
                             (send dc draw-bitmap bmp 0 0))]))

    (define/public (update v what val)
      (case/dep what
        [@path
         (reload! val)
         (send v min-width (send bmp get-width))
         (send v min-height (send bmp get-height))]))

    (define/public (destroy _v)
      (void))

    (define/private (reload! path)
      (set! bmp (call-with-input-file path
                  (λ (in)
                    (make-object gui:bitmap% in 'unknown #f #f scale #f)))))))

(define (image @path)
  (new image%
       [@path (->obs @path)]))
