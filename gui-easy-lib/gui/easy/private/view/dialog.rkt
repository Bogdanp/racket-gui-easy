#lang racket/base

(require racket/class
         (prefix-in gui: racket/gui)
         racket/match
         "../observable.rkt"
         "container.rkt"
         "view.rkt")

(provide
 dialog)

(define dialog%
  (class* container% (view<%>)
    (inherit-field children)
    (init-field @label @size @position style)
    (inherit children-for-dep add-child get-children get-child remove-child)
    (super-new)

    (define/public (dependencies)
      (list @label @size @position))

    (define/public (create parent)
      (define size (obs-peek @size))
      (define position (obs-peek @position))
      (define-values (x y)
        (if (eq? position 'center)
            (values #f #f)
            (values (car position) (cdr position))))
      (define the-dialog
        (new gui:dialog%
             [parent parent]
             [label (obs-peek @label)]
             [width (car size)]
             [height (car size)]
             [x x]
             [y y]
             [style style]))
      (when (eq? position 'center)
        (send the-dialog center))
      (begin0 the-dialog
        (for ([c (in-list children)])
          (add-child c (send c create the-dialog)))
        (send the-dialog show #t)))

    (define/public (update v what val)
      (when (eq? what @label)
        (send v set-label val))
      (when (eq? what @size)
        (send/apply v resize val))
      (when (eq? what @position)
        (match val
          ['center (send v center 'both)]
          [(cons x y) (send v move x y)]))
      (for ([c (in-list (children-for-dep what))])
        (send c update (get-child c) what val)))

    (define/public (destroy v)
      (send v show #f)
      (for ([(c v) (in-hash (get-children))])
        (send c destroy v)
        (remove-child c)))))

(define (dialog #:label [@label (obs "Untitled")]
                #:size [@size (obs (cons 100 100))]
                #:position [@position (obs 'center)]
                #:style [style '(close-button)]
                . children)
  (new dialog%
       [@label (->obs @label)]
       [@size (->obs @size)]
       [@position (->obs @position)]
       [style style]
       [children children]))
