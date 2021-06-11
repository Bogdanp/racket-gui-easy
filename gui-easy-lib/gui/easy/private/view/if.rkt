#lang racket/base

(require (for-syntax racket/base)
         racket/class
         (prefix-in gui: racket/gui)
         racket/list
         syntax/parse/define
         "../observable.rkt"
         "container.rkt"
         "view.rkt")

(provide
 if/view
 cond/view)

(define if-view%
  (class* container% (view<%>)
    (init-field @cond-e then-view else-view)
    (inherit add-child-frame get-child-frame has-child-frame? remove-child-frame)
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
             [parent parent]))
      (begin0 the-pane
        (if (obs-peek @cond-e)
            (add-child-frame then-view (send then-view create the-pane))
            (add-child-frame else-view (send else-view create the-pane)))))

    (define/public (update v what val)
      (when (eq? what @cond-e)
        (when (and val (has-child-frame? else-view))
          (define c-f (get-child-frame else-view))
          (send else-view destroy c-f)
          (send v delete-child c-f)
          (remove-child-frame else-view)
          (add-child-frame then-view (send then-view create v)))
        (when (and (not val) (has-child-frame? then-view))
          (define c-f (get-child-frame then-view))
          (send then-view destroy c-f)
          (send v delete-child c-f)
          (remove-child-frame then-view)
          (add-child-frame else-view (send else-view create v))))

      (when (and (has-child-frame? then-view) (memq what (send then-view dependencies)))
        (send then-view update (get-child-frame then-view) what val))
      (when (and (has-child-frame? else-view) (memq what (send else-view dependencies)))
        (send else-view update (get-child-frame else-view) what val)))

    (define/public (destroy _v)
      (when (and (has-child-frame? then-view))
        (send then-view destroy (get-child-frame then-view))
        (remove-child-frame then-view))
      (when (and (has-child-frame? else-view))
        (send else-view destroy (get-child-frame else-view))
        (remove-child-frame else-view)))))

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
