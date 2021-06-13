#lang racket/base

(require racket/class
         (prefix-in gui: racket/gui)
         racket/match
         "../observable.rkt"
         "common.rkt"
         "view.rkt")

(provide
 table)

(define table%
  (class* object% (view<%>)
    (init-field @label @enabled? @entries @selection @margin @min-size @stretch
                columns style font action)
    (super-new)

    (define single?
      (memq 'single style))

    (define/public (dependencies)
      (list @label @enabled? @entries @selection @margin @min-size @stretch))

    (define/public (create parent)
      (match-define (list h-m v-m) (obs-peek @margin))
      (match-define (list min-w min-h) (obs-peek @min-size))
      (match-define (list w-s? h-s?) (obs-peek @stretch))
      (define selection (obs-peek @selection))
      (define the-list-box
        (new gui:list-box%
             [parent parent]
             [label (obs-peek @label)]
             [choices null]
             [style style]
             [font font]
             [columns columns]
             [enabled (obs-peek @enabled?)]
             [callback (λ (self event)
                         (case (send event get-event-type)
                           [(list-box)
                            (action 'select (if single?
                                                (send self get-selection)
                                                (send self get-selectoins)))]

                           [(list-box-dclick)
                            (action 'dclick (send self get-selection))]

                           [(list-box-column)
                            (action 'column (send event get-column))]))]
             [horiz-margin h-m]
             [vert-margin v-m]
             [min-width min-w]
             [min-height min-h]
             [stretchable-width w-s?]
             [stretchable-height h-s?]))
      (begin0 the-list-box
        (set the-list-box (obs-peek @entries))
        (when selection
          (select the-list-box selection))))

    (define/public (update v what val)
      (case/dep what
        [@label (send v set-label v)]
        [@entries (set v val)]
        [@enabled? (send v enable val)]
        [@selection (select v val)]
        [@margin
         (match-define (list h v) val)
         (send* v
           (horiz-margin h)
           (vert-margin v))]
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
      (void))

    (define/private (set target entries)
      (define entries-by-column
        (for/list ([idx (in-naturals)]
                   [_ (in-list columns)])
          (for/list ([row (in-vector entries)])
            (if (> (vector-length row) idx)
                (vector-ref row idx)
                ""))))
      (send/apply target set entries-by-column))

    (define/private (select target selection)
      (cond
        [single?
         (cond
           [selection
            (send target select selection)]

           [(send target get-selection)
            => (λ (actual-selection)
                 (send target select actual-selection #f))])]
        [else
         (for ([idx (in-list (send target get-selections))])
           (send target select idx #f))
         (for ([idx (in-list selection)])
           (send target select idx))]))))

(define (table columns @entries action
               #:label [@label (obs #f)]
               #:selection [@selection (obs #f)]
               #:enabled? [@enabled? (obs #t)]
               #:style [style '(single column-headers clickable-headers reorderable-headers)]
               #:font [font gui:view-control-font]
               #:margin [@margin (obs '(0 0))]
               #:min-size [@min-size (obs '(#f #f))]
               #:stretch [@stretch (obs '(#t #t))])
  (new table%
       [@label (->obs @label)]
       [@selection (->obs @selection)]
       [@enabled? (->obs @enabled?)]
       [@entries (->obs @entries)]
       [@margin (->obs @margin)]
       [@min-size (->obs @min-size)]
       [@stretch (->obs @stretch)]
       [style style]
       [font font]
       [columns columns]
       [action action]))
