#lang racket/base

(require box-extra
         racket/class
         (prefix-in gui: racket/gui)
         "logger.rkt"
         "observable.rkt"
         "view/window.rkt")

(provide
 render
 render-popup-menu
 renderer<%>)

(define id-seq (box 0))
(define update-id-seq! (make-box-update-proc id-seq))
(define (next-id!)
  (update-id-seq! add1))

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
  (define root (send r render (and parent (renderer-root parent))))
  (log-gui-easy-debug "rendered window ~a" id)
  (begin0 r
    (hash-set! renderers id r)
    (send root show #t)
    (when (is-a? tree dialog%)
      (send r destroy)
      (log-gui-easy-debug "destroyed window ~a" id))))

(define (render-popup-menu r tree x y)
  (define id (next-id!))
  (define menu-r (new renderer% [id id] [tree tree]))
  (define menu (send menu-r render #f))
  (define window (send r get-root))
  (log-gui-easy-debug "rendered popup menu ~a" id)
  (send window popup-menu menu x y)
  (send menu-r destroy)
  (log-gui-easy-debug "destroyed popup menu ~a" id))

(define (renderer-root r)
  (send r get-root))
