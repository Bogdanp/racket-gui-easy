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

    (define choices (peek @choices))

    (define/public (dependencies)
      (filter obs? (remove-duplicates
                    (append (list @choices @selection-index @alignment @enabled? @spacing @margin @min-size @stretch)
                            (child-dependencies)))))

    (define/public (create parent)
      (match-define (list h-m v-m) (peek @margin))
      (match-define (list w h) (peek @min-size))
      (match-define (list w-s? h-s?) (peek @stretch))
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
                 (action 'close choices index))
               (define/override (on-new-request)
                 (action 'new choices (get-selection))))
             [parent parent]
             [choices (map choice->label choices)]
             [callback (λ (self _event)
                         (action 'select choices (send self get-selection)))]
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
      (send the-panel set-selection (peek @selection-index))
      (begin0 the-panel
        (send the-panel begin-container-sequence)
        (for ([c (in-list children)])
          (add-child c (send c create the-panel)))
        (send the-panel end-container-sequence)))

    (define/public (update v what val)
      (case/dep what
        [@choices
         (cond
           ;; The choices are the same and in the same order, so do
           ;; nothing to avoid triggering an unnecessary `select'
           ;; action.
           [(equal? choices val)
            (void)]

           ;; The choices are the same except for the very last one.
           ;; Assume this means we need to append and select the new
           ;; choice.
           [(and (not (null? val)) (equal? choices (drop-right val 1)))
            (set! choices val)
            (send v append (choice->label (last val)))
            (send v set-selection (sub1 (length val)))]

           ;; Otherwise, update the choices and try to preserve the
           ;; current selection if it's a part of the new set.
           [else
            (define selection (send v get-selection))
            (define old-choice (and selection (list-ref choices selection)))
            (set! choices val)
            (send v set (map choice->label val))
            (define index (index-of choices old-choice))
            (when (and selection index)
              (send v set-selection index))])]
        [@selection-index
         (when val
           (unless (eqv? (send v get-selection) val)
             (send v set-selection val)))]
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
              #:selection [@selection-index #f]
              #:alignment [@alignment '(left center)]
              #:enabled? [@enabled? #t]
              #:style [style null]
              #:spacing [@spacing 0]
              #:margin [@margin '(0 0)]
              #:min-size [@min-size '(#f #f)]
              #:stretch [@stretch '(#t #t)]
              . children)
  (new tabs%
       [@choices @choices]
       [@selection-index @selection-index]
       [@alignment @alignment]
       [@enabled? @enabled?]
       [@spacing @spacing]
       [@margin @margin]
       [@min-size @min-size]
       [@stretch @stretch]
       [children children]
       [style style]
       [action action]
       [choice->label choice->label]))
