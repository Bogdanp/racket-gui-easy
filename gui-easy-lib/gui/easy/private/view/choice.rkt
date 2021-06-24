#lang racket/base

(require racket/class
         (prefix-in gui: racket/gui)
         racket/list
         racket/match
         "../observable.rkt"
         "common.rkt"
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
      (filter obs? (list @label @enabled? @choices @selection-index @min-size @stretch)))

    (define/public (create parent)
      (match-define (list min-w min-h) (peek @min-size))
      (match-define (list w-s? h-s?) (peek @stretch))
      (define selection (peek @selection-index))
      (define the-choice
        (new gui:choice%
             [parent parent]
             [label (peek @label)]
             [style style]
             [choices (peek @choices)]
             [enabled (peek @enabled?)]
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
      (case/dep what
        [@choices
         (send v clear)
         (for ([c (in-list val)])
           (send v append c))]
        [@selection-index
         (cond
           [val (send v set-selection val)]
           [(send v get-string-selection) => action]
           [else (action "")])]
        [@label
         (send v set-label val)]
        [@enabled?
         (send v enable val)]
        [@min-size
         (match-define (list w h) val)
         (send* v
           (min-width w)
           (min-height h))]
        [@stretch
         (match-define (list w-s? h-s?) val)
         (send* v
           (stretchable-width w-s?)
           (stretchable-height h-s?))]))

    (define/public (destroy _v)
      (void))))

(define (choice @choices action
                #:selection [@selection 0]
                #:label [@label #f]
                #:style [style null]
                #:enabled? [@enabled? #t]
                #:min-size [@min-size '(#f #f)]
                #:stretch [@stretch '(#f #f)])
  (new choice%
       [@choices (->obs @choices)]
       [@selection (->obs @selection)]
       [@label @label]
       [@enabled? @enabled?]
       [@min-size @min-size]
       [@stretch @stretch]
       [style style]
       [action action]))
