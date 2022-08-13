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
 vpanel
 group)

(define (panel% clazz)
  (class* container% (view<%>)
    (inherit-field children)
    (init-field @alignment @enabled? @spacing @margin @min-size @stretch style)
    (inherit child-dependencies add-child get-child update-children destroy-children)
    (super-new)

    (define/public (dependencies)
      (filter obs? (remove-duplicates
                    (append (list @alignment @enabled? @spacing @margin @min-size @stretch)
                            (child-dependencies)))))

    (define/public (-make-panel parent)
      (match-define (list h-m v-m) (peek @margin))
      (match-define (list w h) (peek @min-size))
      (match-define (list w-s? h-s?) (peek @stretch))
      (new (context-mixin clazz)
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

    (define/public (create parent)
      (define the-panel (-make-panel parent))
      (begin0 the-panel
        (send the-panel begin-container-sequence)
        (for ([c (in-list children)])
          (add-child the-panel c (send c create the-panel)))
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
      (update-children v what val))

    (define/public (destroy v)
      (for ([c (in-list children)])
        (send v delete-child (get-child v c)))
      (destroy-children v))))

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

(define group-box-panel%
  (class* (panel% gui:group-box-panel%) (view<%>)
    (init-field @label)
    (inherit-field @alignment @enabled? @spacing @margin @min-size @stretch style)
    (super-new)

    (define/override (dependencies)
      (define deps (super dependencies))
      (if (obs? @label)
          (cons @label deps)
          deps))

    (define/override (-make-panel parent)
      (match-define (list h-m v-m) (peek @margin))
      (match-define (list w h) (peek @min-size))
      (match-define (list w-s? h-s?) (peek @stretch))
      (new (context-mixin gui:group-box-panel%)
           [label (peek @label)]
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

    (define/override (update v what val)
      (case/dep what
        [@label (send v set-label val)])
      (super update v what val))))

(define (group @label
               #:alignment [@alignment '(center top)]
               #:enabled? [@enabled? #t]
               #:style [style null]
               #:spacing [@spacing 0]
               #:margin [@margin '(0 0)]
               #:min-size [@min-size '(#f #f)]
               #:stretch [@stretch '(#t #t)]
               . children)
  (new group-box-panel%
       [@label @label]
       [@alignment @alignment]
       [@enabled? @enabled?]
       [@spacing @spacing]
       [@margin @margin]
       [@min-size @min-size]
       [@stretch @stretch]
       [children children]
       [style style]))
