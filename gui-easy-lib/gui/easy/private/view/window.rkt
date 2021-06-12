#lang racket/base

(require racket/class
         (prefix-in gui: racket/gui)
         racket/list
         racket/match
         "../observable.rkt"
         "common.rkt"
         "container.rkt"
         "view.rkt")

(provide
 dialog
 window)

(define (window-like% clazz)
  (class* container% (view<%>)
    (inherit-field children)
    (init-field @title @size @alignment @position @min-size @stretch style)
    (inherit child-dependencies add-child update-children destroy-children)
    (super-new)

    (define/public (dependencies)
      (remove-duplicates
       (append (list @title @size @alignment @position @min-size @stretch)
               (child-dependencies))))

    (define/public (create parent)
      (match-define (list w h) (obs-peek @size))
      (match-define (list min-w min-h) (obs-peek @min-size))
      (match-define (list w-s? h-s?) (obs-peek @stretch))
      (define position (obs-peek @position))
      (define-values (x y)
        (match position
          ['center (values #f #f)]
          [(list x y) (values x y)]))
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
      (case/dep what
        [@title (send v set-label val)]
        [@size
         (match-define (list w h) val)
         (send v resize w h)]
        [@alignment
         (send v set-alignment val)]
        [@position
         (match val
           ['center (send v center 'both)]
           [(list x y) (send v move x y)])]
        [@min-size
         (match-define (list w h) val)
         (send* v
           (min-width w)
           (min-height h))]
        [@stretch
         (match-define (list w-s? h-s?) val)
         (send* v
           (stretchable-width w-s?)
           (stretchable-height h-s?))])
      (update-children what val))

    (define/public (destroy v)
      (destroy-children)
      (send v show #f))))

(define dialog% (window-like% gui:dialog%))
(define window% (window-like% gui:frame%))

(define (dialog #:title [@title (obs "Untitled")]
                #:size [@size (obs '(#f #f))]
                #:alignment [@alignment (obs '(center top))]
                #:position [@position (obs 'center)]
                #:style [style '(close-button)]
                #:min-size [@min-size (obs '(#f #f))]
                #:stretch [@stretch (obs '(#t #t))]
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
                #:size [@size (obs '(#f #f))]
                #:position [@position (obs 'center)]
                #:alignment [@alignment (obs '(center top))]
                #:min-size [@min-size (obs '(#f #f))]
                #:stretch [@stretch (obs '(#t #t))]
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
