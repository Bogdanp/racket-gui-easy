#lang racket/base

(require racket/class
         (prefix-in gui: racket/gui)
         racket/list
         racket/match
         "../observable.rkt"
         "../renderer.rkt"
         "common.rkt"
         "container.rkt"
         "view.rkt")

(provide
 list-view)

(define (make-list-view% mix)
  (class* container% (view<%>)
    (init-field @entries @alignment @enabled? @spacing @margin @min-size @stretch
                make-view style key-proc)
    (inherit add-child get-child has-child? remove-child destroy-children)
    (super-new [children null])

    (define (make-keyed-obs k last-e)
      (obs-map @entries (λ (entries)
                          (define new-e
                            (findf
                             (λ (e)
                               (equal? (key-proc e) k))
                             entries))
                          (cond
                            [new-e
                             (begin0 new-e
                               (set! last-e new-e))]
                            [else last-e]))))

    (define (add-child-handlers! v child-v)
      (define renderer (current-renderer))
      (define deps-to-handlers
        (get-deps-to-handlers v))
      (for ([dep (in-list (send child-v dependencies))])
        (define (handle val)
          (gui:queue-callback
           (lambda ()
             (when (has-child? v child-v)
               (parameterize ([current-renderer renderer])
                 (send child-v update (get-child v child-v) dep val))))))
        (obs-observe! dep handle)
        (hash-update! deps-to-handlers
                      (cons child-v dep)
                      (λ (handlers)
                        (cons handle handlers))
                      null)))

    (define (remove-child-handlers! v child-v)
      (define deps-to-handlers
        (get-deps-to-handlers v))
      (for* ([(c&dep hdls) (in-hash deps-to-handlers)]
             [c (in-value (car c&dep))] #:when (eq? child-v c)
             [dep (in-value (cdr c&dep))])
        (for ([hdl (in-list hdls)])
          (obs-unobserve! dep hdl))
        (hash-remove! deps-to-handlers c&dep)))

    (define (remove-all-child-handlers! v)
      (define deps-to-handlers
        (get-deps-to-handlers v))
      (for* ([(c&dep hdls) (in-hash deps-to-handlers)]
             [dep (in-value (cdr c&dep))]
             [hdl (in-list hdls)])
        (obs-unobserve! dep hdl))
      (hash-clear! deps-to-handlers))

    (define/public (dependencies)
      (filter obs? (list @entries @alignment @enabled? @spacing @margin @min-size @stretch)))

    (define/public (create parent)
      (match-define (list h-m v-m) (peek @margin))
      (match-define (list w h) (peek @min-size))
      (match-define (list w-s? h-s?) (peek @stretch))
      (define the-panel
        (new (mix
              (context-mixin
               (if (memq 'vertical style)
                   gui:vertical-panel%
                   gui:horizontal-panel%)))
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
      (define keys-to-children
        (get-keys-to-children the-panel))
      (begin0 the-panel
        (with-container-sequence the-panel
          (for ([e (in-list (peek @entries))])
            (define k (key-proc e))
            (define v (make-view k (make-keyed-obs k e)))
            (define w (send v create the-panel))
            (add-child-handlers! the-panel v)
            (add-child the-panel v w)
            (hash-set! keys-to-children k v)))))

    (define/public (update v what val)
      (case/dep what
        [@entries
         (with-container-sequence v
           (define keys-to-children
             (get-keys-to-children v))
           (define new-keys
             (for/list ([e (in-list val)])
               (define k (key-proc e))
               (begin0 k
                 (unless (hash-has-key? keys-to-children k)
                   (define child-v (make-view k (make-keyed-obs k e)))
                   (define child-w (send child-v create v))
                   (add-child-handlers! v child-v)
                   (add-child v child-v child-w)
                   (hash-set! keys-to-children k child-v)))))
           (for ([(old-k old-v) (in-hash keys-to-children)])
             (unless (member old-k new-keys)
               (define old-w (get-child v old-v))
               (define focused? (send old-w has-focus?))
               (send old-v destroy old-w)
               (send v delete-child old-w)
               (remove-child-handlers! v old-v)
               (remove-child v old-v)
               (hash-remove! keys-to-children old-k)
               (when focused?
                 (define children (send v get-children))
                 (cond
                   [(null? children) (send v focus)]
                   [else (send (last children) focus)]))))
           (send v change-children
                 (λ (_)
                   (for/list ([k (in-list new-keys)])
                     (get-child v (hash-ref keys-to-children k))))))]
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

    (define/public (destroy v)
      (remove-all-child-handlers! v)
      (destroy-children v))

    (define/private (get-keys-to-children v)
      (send v get-context! 'keys-to-children make-hash))
    (define/private (get-deps-to-handlers v)
      (send v get-context! 'deps-to-handlers make-hash))))

(define (list-view @entries make-view
                   #:alignment [@alignment '(left top)]
                   #:enabled? [@enabled? #t]
                   #:style [style '(vertical auto-vscroll)]
                   #:spacing [@spacing 0]
                   #:margin [@margin '(0 0)]
                   #:min-size [@min-size '(#f #f)]
                   #:stretch [@stretch '(#t #t)]
                   #:key [key-proc values]
                   #:mixin [mix values])
  (new (make-list-view% mix)
       [@entries (->obs @entries)]
       [@alignment @alignment]
       [@enabled? @enabled?]
       [@spacing @spacing]
       [@margin @margin]
       [@min-size @min-size]
       [@stretch @stretch]
       [make-view make-view]
       [style style]
       [key-proc key-proc]))
