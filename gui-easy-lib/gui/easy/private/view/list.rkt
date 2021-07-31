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
                make-view style key-proc)
    (inherit add-child get-child has-child? remove-child destroy-children)
    (super-new [children null])

    (define keys-to-children (make-hash))
    (define deps-to-handlers (make-hash))

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

    (define (add-child-handlers! child-v)
      (for ([dep (in-list (send child-v dependencies))])
        (define (handle val)
          (gui:queue-callback
           (λ () (handle-update! child-v dep val)) #f))
        (obs-observe! dep handle)
        (hash-update! deps-to-handlers
                      (cons child-v dep)
                      (λ (handlers)
                        (cons handle handlers))
                      null)))

    (define (remove-child-handlers! child-v)
      (for* ([(c&dep hdls) (in-hash deps-to-handlers)]
             [c (in-value (car c&dep))] #:when (eq? child-v c)
             [dep (in-value (cdr c&dep))])
        (for ([hdl (in-list hdls)])
          (obs-unobserve! dep hdl))
        (hash-remove! deps-to-handlers c&dep)))

    (define (remove-all-child-handlers!)
      (for* ([(c&dep hdls) (in-hash deps-to-handlers)]
             [dep (in-value (cdr c&dep))]
             [hdl (in-list hdls)])
        (obs-unobserve! dep hdl))
      (hash-clear! deps-to-handlers))

    (define flush-frequency 16) ;; ms
    (define flush-scheduled? #f)
    (define pending-updates null)
    (define (handle-update! v what val)
      (set! pending-updates (cons (list v what val) pending-updates))
      (unless flush-scheduled?
        (set! flush-scheduled? #t)
        (define deadline
          (alarm-evt (+ (current-inexact-milliseconds) flush-frequency)))
        (thread
         (lambda ()
           (sync deadline)
           (gui:queue-callback flush-updates!)))))
    (define (flush-updates!)
      (for ([update (in-list pending-updates)])
        (match-define (list v what val) update)
        (when (has-child? v)
          (send v update (get-child v) what val)))
      (set! pending-updates null)
      (set! flush-scheduled? #f))

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
          (define v (make-view (make-keyed-obs k e)))
          (define w (send v create the-panel))
          (add-child-handlers! v)
          (add-child v w)
          (hash-set! keys-to-children k v))
        (send the-panel end-container-sequence)))

    (define/public (update v what val)
      (case/dep what
        [@entries
         (send v begin-container-sequence)
         (define new-keys
           (for/list ([e (in-list val)])
             (define k (key-proc e))
             (begin0 k
               (unless (hash-has-key? keys-to-children k)
                 (define child-v (make-view (make-keyed-obs k e)))
                 (define child-w (send child-v create v))
                 (add-child-handlers! child-v)
                 (add-child child-v child-w)
                 (hash-set! keys-to-children k child-v)))))
         (for ([(old-k old-v) (in-hash keys-to-children)])
           (unless (member old-k new-keys)
             (define old-w (get-child old-v))
             (define focused? (send old-w has-focus?))
             (send old-v destroy old-w)
             (send v delete-child old-w)
             (remove-child-handlers! old-v)
             (remove-child old-v)
             (hash-remove! keys-to-children old-k)
             (when focused?
               (define children (send v get-children))
               (cond
                 [(null? children) (send v focus)]
                 [else (send (last children) focus)]))))
         (send v change-children
               (λ (_)
                 (for/list ([k (in-list new-keys)])
                   (get-child (hash-ref keys-to-children k)))))
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
      (remove-all-child-handlers!)
      (destroy-children))))

(define (list-view @entries make-view
                   #:alignment [@alignment '(left top)]
                   #:enabled? [@enabled? #t]
                   #:style [style '(vertical auto-vscroll)]
                   #:spacing [@spacing 0]
                   #:margin [@margin '(0 0)]
                   #:min-size [@min-size '(#f #f)]
                   #:stretch [@stretch '(#t #t)]
                   #:key [key-proc values])
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
       [key-proc key-proc]))