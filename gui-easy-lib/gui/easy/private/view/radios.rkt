#lang racket/base

(require racket/class
         (prefix-in gui: racket/gui)
         racket/list
         racket/match
         "../observable.rkt"
         "common.rkt"
         "view.rkt")

(provide
 radios)

(define radios%
  (class* object% (view<%>)
    (init-field @label @enabled? @selection @min-size @stretch style choices choice->label choice=? action)
    (super-new)

    (define @selection-index
      (obs-map @selection (λ (selection)
                            (and selection (index-of choices selection choice=?)))))

    (define/public (dependencies)
      (filter obs? (list @label @enabled? @selection-index @min-size @stretch)))

    (define/public (create parent)
      (match-define (list min-w min-h) (peek @min-size))
      (match-define (list w-s? h-s?) (peek @stretch))
      (define selection (peek @selection-index))
      (define the-radio-box
        (new gui:radio-box%
             [parent parent]
             [label (peek @label)]
             [style style]
             [choices (map choice->label choices)]
             [enabled (peek @enabled?)]
             [callback (λ (self _event)
                         (define idx (send self get-selection))
                         (action (and idx (list-ref choices idx))))]
             [min-width min-w]
             [min-height min-h]
             [stretchable-width w-s?]
             [stretchable-height h-s?]))
      (begin0 the-radio-box
        (when selection
          (send the-radio-box set-selection selection))))

    (define/public (update v what val)
      (case/dep what
        [@selection-index
         (cond
           [(null? choices)
            (action #f)]
           [(and val (< val (send v get-number)))
            (unless (equal? val (send v get-selection))
              (send v set-selection val))]
           [else
            (when (send v get-selection)
              (send v set-selection #f)
              (action #f))])]
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

(define (radios choices action
                #:choice->label [choice->label values]
                #:choice=? [choice=? equal?]
                #:selection [@selection #f]
                #:label [@label #f]
                #:style [style '(vertical)]
                #:enabled? [@enabled? #t]
                #:min-size [@min-size '(#f #f)]
                #:stretch [@stretch '(#f #f)])
  (new radios%
       [@selection (->obs @selection)]
       [@label @label]
       [@enabled? @enabled?]
       [@min-size @min-size]
       [@stretch @stretch]
       [style style]
       [choices choices]
       [choice->label choice->label]
       [choice=? choice=?]
       [action action]))
