#lang racket/base

(require racket/class
         (prefix-in gui: racket/gui)
         "../observable.rkt"
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

    (define current-view #f)
    (define current-data (obs-peek @data))
    (define/public (create parent)
      (define the-pane
        (new gui:panel%
             [parent parent]
             [min-width #f]
             [min-height #f]
             [stretchable-width #t]
             [stretchable-height #t]))
      (set! current-view (make-view current-data))
      (begin0 the-pane
        (add-child current-view (send current-view create the-pane))))

    (define/public (update v what val)
      (case/dep what
        [@data
         (unless (equal?-proc val current-data)
           (send v begin-container-sequence)
           (define new-view (make-view val))
           (when (has-child? current-view)
             (send current-view destroy (get-child current-view))
             (remove-child current-view))
           (set! current-view new-view)
           (set! current-data val)
           (add-child current-view (send current-view create v))
           (send v end-container-sequence))]))

    (define/public (destroy _v)
      (when (has-child? current-view)
        (send current-view destroy (get-child current-view))))))

(define (dyn-view @data make-view #:equal? [equal?-proc equal?])
  (new dyn-view%
       [@data @data]
       [make-view make-view]
       [equal?-proc equal?-proc]))
