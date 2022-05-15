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
    (init-field @choices @selection @alignment @enabled? @spacing @margin @min-size @stretch style action choice->label choice=?)
    (inherit child-dependencies add-child update-children destroy-children)
    (super-new)

    (define last-choices null)
    (define last-labels null)
    (define last-selection #f)
    (define last-index #f)
    (define @choices&selection&index
      (obs-combine
       (λ (choices selection)
         (list choices selection (and selection (index-of choices selection choice=?))))
       @choices @selection))

    (define/public (dependencies)
      (filter obs? (remove-duplicates
                    (append (list @choices&selection&index @alignment @enabled? @spacing @margin @min-size @stretch)
                            (child-dependencies)))))

    (define/public (create parent)
      (match-define (list h-m v-m) (peek @margin))
      (match-define (list w h) (peek @min-size))
      (match-define (list w-s? h-s?) (peek @stretch))
      (match-define (list choices selection index) (peek @choices&selection&index))
      (define labels (map choice->label choices))
      (define the-panel
        (new (class gui:tab-panel%
               (super-new)
               (define/augment (on-reorder former-indices)
                 (define choices-vec (list->vector last-choices))
                 (define reordered-choices
                   (for/list ([old-index (in-list former-indices)])
                     (vector-ref choices-vec old-index)))
                 (action 'reorder reordered-choices last-selection))
               (define/override (on-close-request index)
                 (define removed-choices
                   (for/list ([idx (in-naturals)]
                              [c (in-list last-choices)]
                              #:unless (= idx index))
                     c))
                 (define num-choices (length removed-choices))
                 (define adjusted-selection
                   (cond
                     [(null? removed-choices) #f]
                     [(not (eqv? index last-index)) last-selection]
                     [(= index num-choices) (last removed-choices)]
                     [(= num-choices 1) (car removed-choices)]
                     [(zero? index) (car removed-choices)]
                     [else (list-ref removed-choices index)]))
                 (action 'close removed-choices adjusted-selection))
               (define/override (on-new-request)
                 (action 'new last-choices last-selection)))
             [parent parent]
             [choices labels]
             [callback (λ (self _event)
                         (define index (send self get-selection))
                         (action 'select last-choices (and index (list-ref last-choices index))))]
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
        (set! last-choices choices)
        (set! last-labels labels)
        (when index
          (set! last-selection selection)
          (set! last-index index)
          (send the-panel set-selection index))
        (send the-panel begin-container-sequence)
        (for ([c (in-list children)])
          (add-child c (send c create the-panel)))
        (send the-panel end-container-sequence)))

    (define/public (update v what val)
      (case/dep what
        [@choices&selection&index
         (match-define (list choices selection index) val)
         (define labels (map choice->label choices))
         (unless (equal? last-labels labels)
           (send v set labels))
         (when index
           (send v set-selection index))
         (set! last-choices choices)
         (set! last-labels labels)
         (set! last-selection selection)
         (set! last-index index)]
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
              #:choice=? [choice=? equal?]
              #:selection [@selection #f]
              #:alignment [@alignment '(left center)]
              #:enabled? [@enabled? #t]
              #:style [style null]
              #:spacing [@spacing 0]
              #:margin [@margin '(0 0)]
              #:min-size [@min-size '(#f #f)]
              #:stretch [@stretch '(#t #t)]
              . children)
  (new tabs%
       [@choices (->obs @choices)]
       [@selection (->obs @selection)]
       [@alignment @alignment]
       [@enabled? @enabled?]
       [@spacing @spacing]
       [@margin @margin]
       [@min-size @min-size]
       [@stretch @stretch]
       [children children]
       [style style]
       [action action]
       [choice->label choice->label]
       [choice=? choice=?]))
