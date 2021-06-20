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
 tabs)

(define tabs%
  (class* container% (view<%>)
    (inherit-field children)
    (init-field @choices @selection-index @alignment @enabled? @spacing @margin @min-size @stretch style action choice->label)
    (inherit child-dependencies add-child update-children destroy-children)
    (super-new)

    (define choices (obs-peek @choices))
    (define choice-strings (map choice->label choices))
    (define ignore-next-cb? #f)
    (define ignore-next-set? #f)

    (define/public (dependencies)
      (remove-duplicates
       (append (list @choices @selection-index @alignment @enabled? @spacing @margin @min-size @stretch)
               (child-dependencies))))

    (define/public (create parent)
      (match-define (list h-m v-m) (obs-peek @margin))
      (match-define (list w h) (obs-peek @min-size))
      (match-define (list w-s? h-s?) (obs-peek @stretch))
      (define the-panel
        (new (class gui:tab-panel%
               (inherit get-selection)
               (super-new)
               (define/augment (on-reorder former-indices)
                 (define choices-vec (list->vector choices))
                 (define reordered-choices
                   (for/list ([old-index (in-list former-indices)])
                     (vector-ref choices-vec old-index)))
                 (set! choices reordered-choices)
                 (action 'reorder choices (get-selection)))
               (define/override (on-close-request index)
                 (action 'close choices index)))
             [parent parent]
             [choices choice-strings]
             [callback (λ (self _event)
                         (unless ignore-next-cb?
                           (set! ignore-next-set? #t)
                           (action 'select choices (send self get-selection)))
                         (set! ignore-next-cb? #f))]
             [alignment (obs-peek @alignment)]
             [enabled (obs-peek @enabled?)]
             [style style]
             [spacing (obs-peek @spacing)]
             [vert-margin v-m]
             [horiz-margin h-m]
             [min-width w]
             [min-height h]
             [stretchable-width w-s?]
             [stretchable-height h-s?]))
      (begin0 the-panel
        (for ([c (in-list children)])
          (add-child c (send c create the-panel)))))

    (define/public (update v what val)
      (case/dep what
        [@choices
         (unless (equal? choices val)
           (set! choices val)
           (send v set (map choice->label val)))]
        [@selection-index
         (unless ignore-next-set?
           (when val
             (set! ignore-next-cb? #t)
             (send v set-selection val)))
         (set! ignore-next-set? #f)]
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

(define (tabs @choices action
              #:choice->label [choice->label values]
              #:selection [@selection-index (obs #f)]
              #:alignment [@alignment (obs '(left center))]
              #:enabled? [@enabled? (obs #t)]
              #:style [style null]
              #:spacing [@spacing (obs 0)]
              #:margin [@margin (obs '(0 0))]
              #:min-size [@min-size (obs '(#f #f))]
              #:stretch [@stretch (obs '(#t #t))]
              . children)
  (new tabs%
       [@choices (->obs @choices)]
       [@selection-index (->obs @selection-index)]
       [@alignment (->obs @alignment)]
       [@enabled? (->obs @enabled?)]
       [@spacing (->obs @spacing)]
       [@margin (->obs @margin)]
       [@min-size (->obs @min-size)]
       [@stretch (->obs @stretch)]
       [children children]
       [style style]
       [action action]
       [choice->label choice->label]))
