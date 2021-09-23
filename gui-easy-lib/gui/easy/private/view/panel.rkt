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
 hpanel
 vpanel)

(define (panel% clazz)
  (class* container% (view<%>)
    (inherit-field children)
    (init-field @alignment @enabled? @spacing @margin @min-size @stretch style)
    (inherit child-dependencies add-child update-children destroy-children)
    (super-new)

    (define/public (dependencies)
      (filter obs? (remove-duplicates
                    (append (list @alignment @enabled? @spacing @margin @min-size @stretch)
                            (child-dependencies)))))

    (define/public (create parent)
      (match-define (list h-m v-m) (peek @margin))
      (match-define (list w h) (peek @min-size))
      (match-define (list w-s? h-s?) (peek @stretch))
      (define the-panel
        (new clazz
             [parent parent]
             [alignment (peek @alignment)]
             [enabled (peek @enabled?)]
             [style style]
             [spacing (peek @spacing)]
             [vert-margin v-m]
             [horiz-margin h-m]
             [min-width w]
             [min-height h]
             [stretchable-width w-s?]
             [stretchable-height h-s?]))
      (begin0 the-panel
        (send the-panel begin-container-sequence)
        (for ([c (in-list children)])
          (add-child c (send c create the-panel)))
        (send the-panel end-container-sequence)))

    (define/public (update v what val)
      (case/dep what
        [@alignment
         (send/apply v set-alignment val)]
        [@enabled?
         (send v enabled val)]
        [@spacing
         (send v spacing val)]
        [@margin
         (match-define (list h-m v-m) val)
         (send* v
           (horiz-margin h-m)
           (vert-margin v-m))]
        [@min-size
         (match-define (list w h) val)
         (send* v
           (min-width (or w 0))
           (min-height (or h 0)))]
        [@stretch
         (match-define (list w-s? h-s?) val)
         (send* v
           (stretchable-width w-s?)
           (stretchable-height h-s?))])
      (update-children what val))

    (define/public (destroy _v)
      (destroy-children))))

(define hpanel% (panel% gui:horizontal-panel%))
(define vpanel% (panel% gui:vertical-panel%))

(define (hpanel #:alignment [@alignment '(left center)]
                #:enabled? [@enabled? #t]
                #:style [style null]
                #:spacing [@spacing 0]
                #:margin [@margin '(0 0)]
                #:min-size [@min-size '(#f #f)]
                #:stretch [@stretch '(#t #t)]
                . children)
  (new hpanel%
       [@alignment @alignment]
       [@enabled? @enabled?]
       [@spacing @spacing]
       [@margin @margin]
       [@min-size @min-size]
       [@stretch @stretch]
       [children children]
       [style style]))

(define (vpanel #:alignment [@alignment '(center top)]
                #:enabled? [@enabled? #t]
                #:style [style null]
                #:spacing [@spacing 0]
                #:margin [@margin '(0 0)]
                #:min-size [@min-size '(#f #f)]
                #:stretch [@stretch '(#t #t)]
                . children)
  (new vpanel%
       [@alignment @alignment]
       [@enabled? @enabled?]
       [@spacing @spacing]
       [@margin @margin]
       [@min-size @min-size]
       [@stretch @stretch]
       [children children]
       [style style]))
