#lang racket/base

(require racket/class
         (prefix-in gui: racket/gui)
         "../renderer.rkt"
         "common.rkt"
         "proxy.rkt"
         "view.rkt")

(provide
 dyn-view)

(define dyn-view%
  (class* object% (view<%>)
    (init-field @data make-view equal?-proc)
    (super-new)

    (define/public (dependencies)
      (list @data))

    (define/public (create parent)
      (define the-pane
        (new (context-mixin gui:panel%)
             [parent parent]
             [min-width #f]
             [min-height #f]
             [stretchable-width #t]
             [stretchable-height #t]))
      (begin0 the-pane
        (create&add-child the-pane (peek @data))))

    (define/public (update v what val)
      (case/dep what
        [@data
         (unless (equal?-proc val (send v get-context 'data gensym))
           (send v begin-container-sequence)
           (remove&destroy-child v)
           (create&add-child v val)
           (send v end-container-sequence))]))

    (define/public (destroy v)
      (remove&destroy-child v)
      (send v clear-context))

    (define (create&add-child pane data)
      (define view (proxy (make-view data)))
      (define deps (send view dependencies))
      (define widget (send view create pane))
      (define depset (send (current-renderer) add-dependencies deps view widget))
      (send pane set-context* 'data data 'view view 'widget widget 'deps depset))

    (define (remove&destroy-child pane)
      (define view
        (send pane get-context 'view #f))
      (when view
        (define deps (send pane get-context 'deps))
        (define widget (send pane get-context 'widget))
        (send (current-renderer) remove-dependencies deps)
        (send view destroy widget)
        (send pane delete-child widget)
        (send pane clear-context)))))

(define (dyn-view @data make-view #:equal? [equal?-proc equal?])
  (new dyn-view%
       [@data @data]
       [make-view make-view]
       [equal?-proc equal?-proc]))
