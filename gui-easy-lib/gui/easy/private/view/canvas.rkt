#lang racket/base

(require racket/class
         (prefix-in gui: racket/gui)
         "../observable.rkt"
         "common.rkt"
         "view.rkt")

(provide
 canvas)

(define canvas%
  (class* object% (view<%>)
    (init-field @input draw)
    (super-new)

    (define input #f)

    (define/public (dependencies)
      (list @input))

    (define/public (create parent)
      (set! input (obs-peek @input))
      (new gui:canvas%
           [parent parent]
           [paint-callback (λ (_self dc)
                             (draw dc input))]))

    (define/public (update v what val)
      (case/dep what
        [@input
         (set! input val)
         (send v refresh)]))

    (define/public (destroy _v)
      (void))))

(define (canvas @input draw)
  (new canvas%
       [@input (->obs @input)]
       [draw draw]))
