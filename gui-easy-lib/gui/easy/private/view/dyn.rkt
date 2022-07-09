#lang racket/base

(require racket/class
         (prefix-in gui: racket/gui)
         "../observable.rkt"
         "../renderer.rkt"
         "common.rkt"
         "container.rkt"
         "view.rkt")

(provide
 dyn-view)

(define dyn-view%
  (class* container% (view<%>)
    (init-field @data make-view equal?-proc)
    (inherit add-child get-child has-child? remove-child)
    (super-new [children null])

    (define/public (dependencies)
      (list @data))

    (define current-data #f)
    (define current-view #f)
    (define current-deps #f)
    (define/public (create parent)
      (define the-pane
        (new gui:panel%
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
         (unless (equal?-proc val current-data)
           (send v begin-container-sequence)
           (when (has-child? current-view)
             (remove&destroy-child v))
           (create&add-child v val)
           (send v end-container-sequence))]))

    (define/public (destroy v)
      (when (has-child? current-view)
        (remove&destroy-child v)))

    (define (create&add-child pane data)
      (set! current-data data)
      (set! current-view (make-view data))
      (define deps (send current-view dependencies))
      (define widget (send current-view create pane))
      (set! current-deps (send (current-renderer) register-dependencies deps current-view widget))
      (add-child current-view widget))

    (define (remove&destroy-child pane)
      (send (current-renderer) unregister-dependencies current-deps)
      (define widget (get-child current-view))
      (send current-view destroy widget)
      (send pane delete-child widget)
      (remove-child current-view)
      (set! current-data #f)
      (set! current-view #f)
      (set! current-deps #f))))

(define (dyn-view @data make-view #:equal? [equal?-proc equal?])
  (new dyn-view%
       [@data @data]
       [make-view make-view]
       [equal?-proc equal?-proc]))
