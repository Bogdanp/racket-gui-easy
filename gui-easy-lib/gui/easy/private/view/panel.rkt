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
 hpanel
 vpanel)

(define (panel% clazz)
  (class* container% (view<%>)
    (inherit-field children)
    (init-field @alignment @enabled? @min-size @stretch style)
    (inherit children-for-dep child-dependencies
             get-children add-child get-child remove-child)
    (super-new)

    (define/public (dependencies)
      (remove-duplicates
       (append (list @alignment @enabled? @min-size @stretch)
               (child-dependencies))))

    (define/public (create parent)
      (match-define (size w h)
        (obs-peek @min-size))
      (match-define (stretch w-s? h-s?)
        (obs-peek @stretch))
      (define the-panel
        (new clazz
             [parent parent]
             [alignment (obs-peek @alignment)]
             [enabled (obs-peek @enabled?)]
             [style style]
             [min-width w]
             [min-height h]
             [stretchable-width w-s?]
             [stretchable-height h-s?]))
      (begin0 the-panel
        (for ([c (in-list children)])
          (add-child c (send c create the-panel)))))

    (define/public (update v what val)
      (when (eq? what @min-size)
        (match-define (size w h) val)
        (send* v
          (min-width (or w 0))
          (min-height (or h 0))))
      (when (eq? what @alignment)
        (send/apply v set-alignment val))
      (when (eq? what @enabled?)
        (send v enabled val))
      (when (eq? what @stretch)
        (match-define (stretch w-s? h-s?) val)
        (send* v
          (stretchable-width w-s?)
          (stretchable-height h-s?)))
      (for ([c (in-list (children-for-dep what))])
        (send c update (get-child c) what val)))

    (define/public (destroy _v)
      (for ([(c w) (in-hash (get-children))])
        (send c destroy w)
        (remove-child c)))))

(define hpanel% (panel% gui:horizontal-panel%))
(define vpanel% (panel% gui:vertical-panel%))

(define (hpanel #:alignment [@alignment (obs '(left center))]
                #:enabled? [@enabled? (obs #t)]
                #:style [style null]
                #:min-size [@min-size (obs (size #f #f))]
                #:stretch [@stretch (obs (stretch #t #t))]
                . children)
  (new hpanel%
       [@alignment (->obs @alignment)]
       [@enabled? (->obs @enabled?)]
       [@min-size (->obs @min-size)]
       [@stretch (->obs @stretch)]
       [children children]
       [style style]))

(define (vpanel #:alignment [@alignment (obs '(center top))]
                #:enabled? [@enabled? (obs #t)]
                #:style [style null]
                #:min-size [@min-size (obs (size #f #f))]
                #:stretch [@stretch (obs (stretch #t #t))]
                . children)
  (new vpanel%
       [@alignment (->obs @alignment)]
       [@enabled? (->obs @enabled?)]
       [@min-size (->obs @min-size)]
       [@stretch (->obs @stretch)]
       [children children]
       [style style]))
