#lang racket/base

(require racket/class
         racket/contract
         (prefix-in gui: racket/gui)
         racket/list
         racket/match
         "../observable.rkt"
         "common.rkt"
         "container.rkt"
         "view.rkt")

(provide
 window-view<%>
 dialog
 window)

(define window-view<%>
  (interface (view<%>)
    [create (->m (or/c (is-a?/c gui:frame%)
                       (is-a?/c gui:dialog%)
                       #f)
                 (is-a?/c gui:top-level-window<%>))]
    [is-dialog? (->m boolean?)]))

(define (window-like% clazz)
  (class* container% (window-view<%>)
    (inherit-field children)
    (init-field @title @size @alignment @position @min-size @stretch style)
    (inherit child-dependencies add-child get-child update-children destroy-children)
    (super-new)

    (define/public (dependencies)
      (filter obs? (remove-duplicates
                    (append (list @title @size @alignment @position @min-size @stretch)
                            (child-dependencies)))))

    (define/public (create parent)
      (match-define (list w h) (peek @size))
      (match-define (list min-w min-h) (peek @min-size))
      (match-define (list w-s? h-s?) (peek @stretch))
      (define position (peek @position))
      (define-values (x y)
        (match position
          ['center (values #f #f)]
          [(list x y) (values x y)]))
      (define the-window
        (new (context-mixin clazz)
             [parent parent]
             [label (peek @title)]
             [alignment (peek @alignment)]
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
          (add-child the-window c (send c create the-window)))))

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
      (update-children v what val))

    (define/public (destroy v)
      (send v change-children (λ (_) null))
      (destroy-children v)
      (send v show #f))

    (define/public (is-dialog?)
      #f)))

(define (make-dialog% mix)
  (class (window-like% (mix gui:dialog%))
    (super-new)
    (define/override (is-dialog?) #t)))

(define (make-window% mix)
  (window-like% (mix gui:frame%)))

(define (dialog #:title [@title "Untitled"]
                #:size [@size '(#f #f)]
                #:alignment [@alignment '(center top)]
                #:position [@position 'center]
                #:style [style '(close-button)]
                #:min-size [@min-size '(#f #f)]
                #:stretch [@stretch '(#t #t)]
                #:mixin [mix values]
                . children)
  (new (make-dialog% mix)
       [@title @title]
       [@size @size]
       [@alignment @alignment]
       [@position @position]
       [@min-size @min-size]
       [@stretch @stretch]
       [style style]
       [children children]))

(define (window #:title [@title "Untitled"]
                #:size [@size '(#f #f)]
                #:position [@position 'center]
                #:alignment [@alignment '(center top)]
                #:min-size [@min-size '(#f #f)]
                #:stretch [@stretch '(#t #t)]
                #:style [style null]
                #:mixin [mix values]
                . children)
  (new (make-window% mix)
       [@title @title]
       [@size @size]
       [@alignment @alignment]
       [@position @position]
       [@min-size @min-size]
       [@stretch @stretch]
       [style style]
       [children children]))
