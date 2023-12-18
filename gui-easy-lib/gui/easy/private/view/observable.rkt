#lang racket/base

(require racket/class
         (prefix-in gui: racket/gui)
         "../renderer.rkt"
         "common.rkt"
         "container.rkt"
         "proxy.rkt"
         "view.rkt")

(provide
 observable-view)

(define observable-view%
  (class* object% (view<%>)
    (init-field @data make-view equal?-proc)
    (super-new)

    (define/public (dependencies)
      (list @data))

    (define/public (create parent)
      (define the-pane
        (new (context-mixin gui:panel%)
             [parent parent]))
      (begin0 the-pane
        (create&add-child the-pane (peek @data))))

    (define/public (update v what val)
      (case/dep what
        [@data
         (unless (equal?-proc val (get-data v))
           (with-container-sequence v
             (remove&destroy-child v)
             (create&add-child v val)))])
      (define child
        (send v get-context 'view #f))
      (when child
        (define child-v (send v get-context 'widget))
        (send child update child-v what val)
        (copy-area<%>-properties child-v v)))

    (define/public (destroy v)
      (remove&destroy-child v)
      (send v clear-context))

    (define/private (get-data pane)
      (send pane get-context 'data gensym))

    (define (create&add-child pane data)
      (define view (proxy (make-view data)))
      (define deps (send view dependencies))
      (define widget (send view create pane))
      (define depset (send (current-renderer) add-dependencies deps this pane))
      (send pane set-context* 'data data 'view view 'widget widget 'deps depset)
      (copy-area<%>-properties widget pane))

    (define (remove&destroy-child pane)
      (define view
        (send pane get-context 'view #f))
      (when view
        (define deps (send pane get-context 'deps))
        (define widget (send pane get-context 'widget))
        (send (current-renderer) remove-dependencies deps)
        (send view destroy widget)
        (send pane delete-child widget)
        (send pane clear-context)))

    (define (copy-area<%>-properties src dst)
      (send dst min-width (send src min-width))
      (send dst min-width (send src min-height))
      (send dst stretchable-width (send src stretchable-width))
      (send dst stretchable-height (send src stretchable-height)))))

(define (observable-view @data
                         [make-view values]
                         #:equal? [equal?-proc equal?])
  (new observable-view%
       [@data @data]
       [make-view make-view]
       [equal?-proc equal?-proc]))
