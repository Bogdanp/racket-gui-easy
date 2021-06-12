#lang racket/base

(require racket/class
         (prefix-in gui: racket/gui)
         racket/list
         racket/match
         "../observable.rkt"
         "view.rkt")

(provide
 choice)

(define choice%
  (class* object% (view<%>)
    (init-field @label @enabled? @choices @selection @min-size @stretch style action)
    (super-new)

    (define @selection-index
      (obs-combine
       (λ (choices selection)
         (index-of choices selection))
       @choices @selection))

    (define/public (dependencies)
      (list @label @enabled? @choices @selection-index @min-size @stretch))

    (define/public (create parent)
      (match-define (list min-w min-h) (obs-peek @min-size))
      (match-define (list w-s? h-s?) (obs-peek @stretch))
      (define selection (obs-peek @selection-index))
      (define the-choice
        (new gui:choice%
             [parent parent]
             [label (obs-peek @label)]
             [style style]
             [choices (obs-peek @choices)]
             [enabled (obs-peek @enabled?)]
             [callback (λ (self _event)
                         (action (send self get-string-selection)))]
             [min-width min-w]
             [min-height min-h]
             [stretchable-width w-s?]
             [stretchable-height h-s?]))
      (begin0 the-choice
        (when selection
          (send the-choice set-selection selection))))

    (define/public (update v what val)
      (when (eq? what @choices)
        (send v clear)
        (for ([c (in-list val)])
          (send v append c)))
      (when (and (eq? what @selection-index) val)
        (send v set-selection val))
      (when (eq? what @label)
        (send v set-label val))
      (when (eq? what @enabled?)
        (send v enable val))
      (when (eq? what @min-size)
        (match-define (list w h) val)
        (send* v
          (min-width w)
          (min-height h)))
      (when (eq? what @stretch)
        (match-define (list w-s? h-s?) val)
        (send* v
          (stretchable-width w-s?)
          (stretchable-height h-s?))))

    (define/public (destroy _v)
      (void))))

(define (choice @choices action
                #:selection [@selection (obs 0)]
                #:label [@label (obs #f)]
                #:style [style null]
                #:enabled? [@enabled? (obs #t)]
                #:min-size [@min-size (obs '(#f #f))]
                #:stretch [@stretch (obs '(#f #f))])
  (new choice%
       [@choices (->obs @choices)]
       [@selection (->obs @selection)]
       [@label (->obs @label)]
       [@enabled? (->obs @enabled?)]
       [@min-size (->obs @min-size)]
       [@stretch (->obs @stretch)]
       [style style]
       [action action]))
