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
 list-view)

(define list-view%
  (class* container% (view<%>)
    (init-field @entries @alignment @enabled? @spacing @margin @min-size @stretch
                make-view style key-proc equal?-proc)
    (inherit add-child get-child remove-child destroy-children)
    (super-new [children null])

    (define keys-to-children (make-hash))
    (define keys-to-entries  (make-hash))

    (define/public (dependencies)
      (filter obs? (list @entries @alignment @enabled? @spacing @margin @min-size @stretch)))

    (define/public (create parent)
      (match-define (list h-m v-m) (peek @margin))
      (match-define (list w h) (peek @min-size))
      (match-define (list w-s? h-s?) (peek @stretch))
      (define the-panel
        (new (if (memq 'vertical style)
                 gui:vertical-panel%
                 gui:horizontal-panel%)
             [parent parent]
             [alignment (peek @alignment)]
             [enabled (peek @enabled?)]
             [style (filter-not (λ (s) (memq s '(vertical horizontal))) style)]
             [spacing (peek @spacing)]
             [vert-margin v-m]
             [horiz-margin h-m]
             [min-width w]
             [min-height h]
             [stretchable-width w-s?]
             [stretchable-height h-s?]))
      (begin0 the-panel
        (send the-panel begin-container-sequence)
        (for ([e (in-list (peek @entries))])
          (define k (key-proc e))
          (define v (make-view e))
          (add-child v (send v create the-panel))
          (hash-set! keys-to-children k v)
          (hash-set! keys-to-entries  k e))
        (send the-panel end-container-sequence)))

    (define/public (update v what val)
      (case/dep what
        [@entries
         (send v begin-container-sequence)
         (define new-keys
           (for/list ([e (in-list val)])
             (define k (key-proc e))
             (begin0 k
               (cond
                 [(hash-ref keys-to-children k #f)
                  => (λ (child-v)
                       (define child-e (hash-ref keys-to-entries k))
                       (unless (equal?-proc e child-e)
                         (define child-w (get-child child-v))
                         (send child-v destroy child-w)
                         (send v delete-child child-w)
                         (remove-child child-v)

                         (define new-child-v (make-view e))
                         (add-child new-child-v (send new-child-v create v))
                         (hash-set! keys-to-children k new-child-v)
                         (hash-set! keys-to-entries k e)))]
                 [else
                  (define child-v (make-view e))
                  (add-child child-v (send child-v create v))
                  (hash-set! keys-to-children k child-v)
                  (hash-set! keys-to-entries k e)]))))
         (for ([(old-k old-v) (in-hash keys-to-children)])
           (unless (member old-k new-keys)
             (define old-w (get-child old-v))
             (send old-v destroy old-w)
             (send v delete-child old-w)
             (remove-child old-v)))
         (for ([w (in-list (send v get-children))])
           (send v delete-child w))
         (for ([k (in-list new-keys)])
           (define child-v (hash-ref keys-to-children k))
           (send v add-child (get-child child-v)))
         (send v end-container-sequence)]
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
           (stretchable-height h-s?))]))

    (define/public (destroy _v)
      (destroy-children))))

(define (list-view @entries make-view
                   #:alignment [@alignment '(left top)]
                   #:enabled? [@enabled? #t]
                   #:style [style '(vertical auto-vscroll)]
                   #:spacing [@spacing 0]
                   #:margin [@margin '(0 0)]
                   #:min-size [@min-size '(#f #f)]
                   #:stretch [@stretch '(#t #t)]
                   #:key [key-proc values]
                   #:equal? [equal?-proc equal?])
  (new list-view%
       [@entries @entries]
       [@alignment @alignment]
       [@enabled? @enabled?]
       [@spacing @spacing]
       [@margin @margin]
       [@min-size @min-size]
       [@stretch @stretch]
       [make-view make-view]
       [style style]
       [key-proc key-proc]
       [equal?-proc equal?-proc]))
