#lang racket/base

(require racket/class
         (prefix-in gui: racket/gui)
         "common.rkt"
         "logger.rkt"
         "observable.rkt"
         "view/window.rkt")

(provide
 render
 renderer<%>
 renderer-root)

(define id-seq (box 0))
(define (next-id!)
  (box-update id-seq add1))

(define renderer<%>
  (interface () get-root render destroy))

(define renderer%
  (class* object% (renderer<%>)
    (init-field id tree)
    (super-new)

    (define root #f)
    (define/public (get-root) root)

    (define deps (send tree dependencies))
    (define handlers null)

    (define/public (render parent)
      (set! root (send tree create parent))
      (set! handlers (for/list ([dep (in-list deps)])
                       (define (f v)
                         (gui:queue-callback
                          (λ ()
                            (send tree update root dep v))
                          #f))
                       (begin0 f
                         (obs-observe! dep f))))
      root)

    (define/public (destroy)
      (for ([dep (in-list deps)]
            [handler (in-list handlers)])
        (obs-unobserve! dep handler))
      (hash-remove! renderers id)
      (send tree destroy root)
      (set! root #f))))

(define renderers (make-hasheqv))
(define (render tree [parent #f])
  (define id (next-id!))
  (define r (new renderer% [id id] [tree tree]))
  (define root (send r render parent))
  (log-gui-easy-debug "rendered renderer ~a" id)
  (begin0 r
    (hash-set! renderers id r)
    (send root show #t)
    (when (is-a? tree dialog%)
      (send r destroy)
      (log-gui-easy-debug "destroyed renderer ~s" id))))

(define (renderer-root r)
  (send r get-root))
