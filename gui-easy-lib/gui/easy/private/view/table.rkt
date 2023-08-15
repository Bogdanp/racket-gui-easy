#lang racket/base

(require racket/class
         (prefix-in gui: racket/gui)
         racket/match
         "../observable.rkt"
         "common.rkt"
         "view.rkt")

(provide
 table)

(define (make-table% %)
  (class* object% (view<%>)
    (init-field @label @enabled? @entries @selection @margin @min-size @stretch @column-widths
                entry->row columns style font action)
    (super-new)

    (define single?
      (memq 'single style))

    (define/public (dependencies)
      (filter obs? (list @label @enabled? @entries @selection @margin @min-size @stretch @column-widths)))

    (define/public (create parent)
      (match-define (list h-m v-m) (peek @margin))
      (match-define (list min-w min-h) (peek @min-size))
      (match-define (list w-s? h-s?) (peek @stretch))
      (define selection (peek @selection))
      (define column-widths (peek @column-widths))
      (define the-list-box
        (new (context-mixin %)
             [parent parent]
             [label (peek @label)]
             [choices null]
             [style style]
             [font font]
             [columns columns]
             [enabled (peek @enabled?)]
             [callback (λ (self event)
                         (define entries
                           (send self get-context 'entries))
                         (case (send event get-event-type)
                           [(list-box)
                            (action 'select entries (if single?
                                                        (send self get-selection)
                                                        (send self get-selections)))]

                           [(list-box-dclick)
                            (action 'dclick entries (send self get-selection))]

                           [(list-box-column)
                            (action 'column entries (send event get-column))]))]
             [horiz-margin h-m]
             [vert-margin v-m]
             [min-width min-w]
             [min-height min-h]
             [stretchable-width w-s?]
             [stretchable-height h-s?]))
      (begin0 the-list-box
        (set the-list-box (peek @entries))
        (when selection
          (select the-list-box selection))
        (resize-columns the-list-box column-widths)))

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
           (stretchable-height h-s?))]
        [@column-widths
         (resize-columns v val)]))

    (define/public (destroy v)
      (send v clear-context))

    (define/private (set target entries)
      (define entries-by-column
        (for/list ([idx (in-naturals)]
                   [_ (in-list columns)])
          (for/list ([entry (in-vector entries)])
            (define row (entry->row entry))
            (if (> (vector-length row) idx)
                (vector-ref row idx)
                ""))))
      (send target set-context 'entries entries)
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
           (send target select idx))]))

    (define/private (resize-columns v widths)
      (for ([spec (in-list widths)])
        (define-values (idx w min-w max-w)
          (match spec
            [`(,idx ,width) (values idx width 0 10000)]
            [`(,idx ,width ,min-width ,max-width) (values idx width min-width max-width)]))
        (send v set-column-width idx w min-w max-w)))))

(define (table columns @entries
               [action void]
               #:entry->row [entry->row values]
               #:label [@label #f]
               #:selection [@selection #f]
               #:enabled? [@enabled? #t]
               #:style [style '(single column-headers clickable-headers reorderable-headers)]
               #:font [font gui:view-control-font]
               #:margin [@margin '(0 0)]
               #:min-size [@min-size '(#f #f)]
               #:stretch [@stretch '(#t #t)]
               #:column-widths [@column-widths null]
               #:mixin [mix values])
  (new (make-table% (mix gui:list-box%))
       [@label @label]
       [@selection @selection]
       [@enabled? @enabled?]
       [@entries  @entries]
       [@margin @margin]
       [@min-size @min-size]
       [@stretch @stretch]
       [@column-widths @column-widths]
       [entry->row entry->row]
       [style style]
       [font font]
       [columns columns]
       [action action]))
