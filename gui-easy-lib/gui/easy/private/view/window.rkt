#lang racket/base

(require racket/class
         (prefix-in gui: racket/gui)
         racket/list
         racket/match
         "../observable.rkt"
         "container.rkt"
         "view.rkt")

(provide
 dialog
 window)

(define (window-like% clazz)
  (class* container% (view<%>)
    (inherit-field children)
    (init-field @title @size @position style)
    (inherit child-dependencies children-for-dep
             get-children add-child get-child remove-child)
    (super-new)

    (define/public (dependencies)
      (remove-duplicates
       (append (list @title @size @position)
               (child-dependencies))))

    (define/public (create parent)
      (define size (obs-peek @size))
      (define position (obs-peek @position))
      (define-values (x y)
        (if (eq? position 'center)
            (values #f #f)
            (values (car position) (cdr position))))
      (define the-window
        (new clazz
             [parent parent]
             [label (obs-peek @title)]
             [style style]
             [width (car size)]
             [height (cdr size)]
             [x x]
             [y y]))
      (when (eq? position 'center)
        (send the-window center 'both))
      (begin0 the-window
        (for ([c (in-list children)])
          (add-child c (send c create the-window)))
        (send the-window show #t)))

    (define/public (update v what val)
      (when (eq? what @title)
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
      (for ([(c w) (in-hash (get-children))])
        (send c destroy w)
        (remove-child c)))))

(define dialog% (window-like% gui:dialog%))
(define window% (window-like% gui:frame%))

(define (dialog #:title [@title (obs "Untitled")]
                #:size [@size (obs (cons 100 100))]
                #:position [@position (obs 'center)]
                #:style [style '(close-button)]
                . children)
  (new dialog%
       [@title (->obs @title)]
       [@size (->obs @size)]
       [@position (->obs @position)]
       [style style]
       [children children]))

(define (window #:title [@title (obs "Untitled")]
                #:size [@size (obs (cons 100 100))]
                #:position [@position (obs 'center)]
                #:style [style null]
                . children)
  (new window%
       [@title (->obs @title)]
       [@size (->obs @size)]
       [@position (->obs @position)]
       [style style]
       [children children]))
