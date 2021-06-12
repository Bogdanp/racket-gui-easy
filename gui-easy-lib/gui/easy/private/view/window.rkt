#lang racket/base

(require racket/class
         (prefix-in gui: racket/gui)
         racket/list
         racket/match
         "../observable.rkt"
         "container.rkt"
         "size.rkt"
         "view.rkt")

(provide
 dialog
 window)

(define (window-like% clazz)
  (class* container% (view<%>)
    (inherit-field children)
    (init-field @title @size @alignment @position @min-size @stretch style)
    (inherit child-dependencies children-for-dep
             get-children add-child get-child remove-child)
    (super-new)

    (define/public (dependencies)
      (remove-duplicates
       (append (list @title @size @alignment @position @min-size @stretch)
               (child-dependencies))))

    (define/public (create parent)
      (match-define (size w h) (obs-peek @size))
      (match-define (size min-w min-h) (obs-peek @min-size))
      (match-define (stretch w-s? h-s?) (obs-peek @stretch))
      (define position (obs-peek @position))
      (define-values (x y)
        (if (eq? position 'center)
            (values #f #f)
            (values (car position) (cdr position))))
      (define the-window
        (new clazz
             [parent parent]
             [label (obs-peek @title)]
             [alignment (obs-peek @alignment)]
             [style style]
             [width w]
             [height h]
             [x x]
             [y y]
             [min-width min-w]
             [min-height min-h]
             [stretchable-width w-s?]
             [stretchable-height h-s?]))
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
        (match-define (size w h) val)
        (send v resize w h))
      (when (eq? what @alignment)
        (send v set-alignment val))
      (when (eq? what @position)
        (match val
          ['center (send v center 'both)]
          [(cons x y) (send v move x y)]))
      (when (eq? what @min-size)
        (match-define (size w h) val)
        (send* v
          (min-width w)
          (min-height h)))
      (when (eq? what @stretch)
        (match-define (stretch w-s? h-s?) val)
        (send* v
          (stretchable-width w-s?)
          (stretchable-height h-s?)))
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
                #:size [@size (obs (size #f #f))]
                #:alignment [@alignment (obs '(center top))]
                #:position [@position (obs 'center)]
                #:style [style '(close-button)]
                #:min-size [@min-size (obs (size #f #f))]
                #:stretch [@stretch (obs (stretch #t #t))]
                . children)
  (new dialog%
       [@title (->obs @title)]
       [@size (->obs @size)]
       [@alignment (->obs @alignment)]
       [@position (->obs @position)]
       [@min-size (->obs @min-size)]
       [@stretch (->obs @stretch)]
       [style style]
       [children children]))

(define (window #:title [@title (obs "Untitled")]
                #:size [@size (obs (size #f #f))]
                #:position [@position (obs 'center)]
                #:alignment [@alignment (obs '(center top))]
                #:min-size [@min-size (obs (size #f #f))]
                #:stretch [@stretch (obs (stretch #t #t))]
                #:style [style null]
                . children)
  (new window%
       [@title (->obs @title)]
       [@size (->obs @size)]
       [@alignment (->obs @alignment)]
       [@position (->obs @position)]
       [@min-size (->obs @min-size)]
       [@stretch (->obs @stretch)]
       [style style]
       [children children]))
