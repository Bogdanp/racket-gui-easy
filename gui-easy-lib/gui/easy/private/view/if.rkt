#lang racket/base

(require (for-syntax racket/base)
         racket/class
         (prefix-in gui: racket/gui)
         racket/list
         racket/match
         syntax/parse/define
         "../observable.rkt"
         "common.rkt"
         "container.rkt"
         "view.rkt")

(provide
 if/view
 cond/view)

(define if-view%
  (class* container% (view<%>)
    (init-field @cond-e then-view else-view)
    (inherit add-child get-child has-child? remove-child)
    (super-new)

    (define/public (dependencies)
      (remove-duplicates
       (append
        (list @cond-e)
        (send then-view dependencies)
        (send else-view dependencies))))

    (define/public (create parent)
      (define the-pane
        (new gui:panel%
             [parent parent]
             [min-width #f]
             [min-height #f]
             [stretchable-width #t]
             [stretchable-height #t]))
      (begin0 the-pane
        (if (obs-peek @cond-e)
            (add-child then-view (send then-view create the-pane))
            (add-child else-view (send else-view create the-pane)))))

    (define/public (update v what val)
      (case/dep what
        [@cond-e
         (when (and val (has-child? else-view))
           (define w (get-child else-view))
           (send else-view destroy w)
           (send v delete-child w)
           (remove-child else-view)
           (add-child then-view (send then-view create v)))
         (when (and (not val) (has-child? then-view))
           (define w (get-child then-view))
           (send then-view destroy w)
           (send v delete-child w)
           (remove-child then-view)
           (add-child else-view (send else-view create v)))])

      (when (and (has-child? then-view) (memq what (send then-view dependencies)))
        (send then-view update (get-child then-view) what val))
      (when (and (has-child? else-view) (memq what (send else-view dependencies)))
        (send else-view update (get-child else-view) what val)))

    (define/public (destroy _v)
      (when (has-child? then-view)
        (send then-view destroy (get-child then-view))
        (remove-child then-view))
      (when (has-child? else-view)
        (send else-view destroy (get-child else-view))
        (remove-child else-view)))))

(define (if/view @cond-e then-view else-view)
  (new if-view%
       [@cond-e (->obs @cond-e)]
       [then-view then-view]
       [else-view else-view]
       [children null]))

(define-syntax-parser cond/view
  #:literals (else)
  [(_ [@cond-e:expr view-e:expr] [else else-e:expr])
   #'(if/view @cond-e view-e else-e)]

  [(_ [@cond-e-1:expr view-e-1:expr] [@cond-e:expr view-e:expr] ... [else else-e:expr])
   #'(if/view @cond-e-1
              view-e-1
              (cond/view
               [@cond-e view-e] ...
               [else else-e]))])
