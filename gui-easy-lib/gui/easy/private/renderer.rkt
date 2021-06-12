#lang racket/base

(require racket/class
         (prefix-in gui: racket/gui)
         "observable.rkt")

(provide
 render
 renderer<%>
 renderer-root)

(define renderer<%>
  (interface () get-root render destroy))

(define renderer%
  (class* object% (renderer<%>)
    (init-field tree)
    (super-new)

    (define root #f)
    (define/public (get-root)
      root)

    (define/public (render parent)
      (define deps (send tree dependencies))
      (set! root (send tree create #f))
      (for ([dep (in-list deps)])
        (define dep-box (make-weak-box dep))
        (obs-observe! dep (λ (v)
                            (gui:queue-callback
                             (λ ()
                               (define maybe-dep (weak-box-value dep-box))
                               (when maybe-dep
                                 (send tree update root maybe-dep v)))
                             #f)))))

    (define/public (destroy)
      (send tree destroy root))))

(define renderers null)
(define (render tree [parent #f])
  (define r (new renderer% [tree tree]))
  (send r render parent)
  (begin0 r
    (set! renderers (cons r renderers))))

(define (renderer-root r)
  (send r get-root))
