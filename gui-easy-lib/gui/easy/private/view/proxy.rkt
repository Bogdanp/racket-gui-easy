#lang racket/base

(require racket/class
         "../logger.rkt"
         "view.rkt")

(provide
 proxy)

;; A proxy for views with dynamic dependencies (children of if-view
;; and dyn-view).  It keeps track of whether the target view has
;; already been destroyed, in which case it stops sending updates
;; downstream.  This is necessary because pending changes can be
;; queued up at the eventspace layer even after the view has been
;; replaced.
(define proxy%
  (class* object% (view<%>)
    (init-field target)
    (super-new)

    (define destroyed? #f)

    (define/public (dependencies)
      (send target dependencies))

    (define/public (create parent)
      (send target create parent))

    (define/public (update v what val)
      (unless destroyed?
        (send target update v what val)))

    (define/public (destroy v)
      (cond
        [destroyed?
         (log-gui-easy-warning "view ~e destroyed a second time" v)]
        [else
         (send target destroy v)
         (set! destroyed? #t)
         (set! target #f)]))))

(define (proxy target)
  (new proxy% [target target]))
