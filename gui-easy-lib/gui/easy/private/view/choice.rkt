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
    (init-field @label @enabled? @choices @selection @min-size @stretch style choice->label choice=? action)
    (super-new)

    (define @choices&index
      (obs-combine
       (λ (choices selection)
         (list choices (and selection (index-of choices selection choice=?))))
       @choices @selection))

    (define (choices=? cs1 cs2)
      (and (= (length cs1)
              (length cs2))
           (for/and ([c1 (in-list cs1)]
                     [c2 (in-list cs2)])
             (choice=? c1 c2))))

    (define/public (dependencies)
      (filter obs? (list @label @enabled? @choices&index @min-size @stretch)))

    (define/public (create parent)
      (match-define (list min-w min-h) (peek @min-size))
      (match-define (list w-s? h-s?) (peek @stretch))
      (match-define (list choices selection) (peek @choices&index))
      (define the-choice
        (new (context-mixin gui:choice%)
             [parent parent]
             [label (peek @label)]
             [style style]
             [choices (map choice->label choices)]
             [enabled (peek @enabled?)]
             [callback (λ (self _event)
                         (define idx (send self get-selection))
                         (define last-choices (send self get-context 'last-choices))
                         (action (and idx (list-ref last-choices idx))))]
             [min-width min-w]
             [min-height min-h]
             [stretchable-width w-s?]
             [stretchable-height h-s?]))
      (begin0 the-choice
        (send the-choice set-context 'last-choices choices)
        (when selection
          (send the-choice set-context 'last-selection (send the-choice get-string-selection))
          (send the-choice set-selection selection))))

    (define/public (update v what val)
      (case/dep what
        [@choices&index
         (match-define (list choices index) val)
         (define last-choices (send v get-context 'last-choices))
         (define last-selection (send v get-context 'last-selection #f))
         (unless (choices=? choices last-choices)
           (send v clear)
           (for ([i (in-naturals)]
                 [c (in-list choices)])
             (define lbl (choice->label c))
             (send v append lbl)
             (when (equal? lbl last-selection)
               (send v set-selection i))))
         (cond
           [(null? choices)
            (unless (null? last-choices)
              (send v set-context 'last-selection #f)
              (action #f))]
           [(and index (< index (send v get-number)))
            (define current-selection (send v get-string index))
            (unless (equal? current-selection last-selection)
              (send v set-context 'last-selection current-selection)
              (send v set-selection index))]
           [else
            (define current-selection (send v get-string-selection))
            (define current-selection-idx (send v get-selection))
            (unless (equal? current-selection last-selection)
              (send v set-context 'last-selection current-selection)
              (when (< current-selection-idx (length choices))
                (action (list-ref choices current-selection-idx))))])
         (send v set-context 'last-choices choices)]
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

    (define/public (destroy v)
      (send v clear-context))))

(define (choice @choices action
                #:choice->label [choice->label values]
                #:choice=? [choice=? equal?]
                #:selection [@selection #f]
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
       [choice->label choice->label]
       [choice=? choice=?]
       [action action]))
