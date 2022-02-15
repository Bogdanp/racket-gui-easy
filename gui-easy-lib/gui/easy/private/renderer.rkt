#lang racket/base

(require box-extra
         racket/class
         (prefix-in gui: racket/gui)
         "logger.rkt"
         "observable.rkt")

(provide
 embed

 render
 render-popup-menu

 renderer<%>
 renderer-root)

(define id-seq (box 0))
(define update-id-seq! (make-box-update-proc id-seq))
(define (next-id!)
  (update-id-seq! add1))

(define renderers (make-hasheqv))
(define renderer<%>
  (interface () get-root render destroy))

(define renderer%
  (class* object% (renderer<%>)
    (init-field tree)
    (field [id (next-id!)])
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

(define (embed parent tree)
  (define r (new renderer% [tree tree]))
  (define id (get-field id r))
  (send r render parent)
  (log-gui-easy-debug "rendered tree ~a" id)
  (begin0 r
    (hash-set! renderers id r)))

(define (render tree [parent #f])
  (define r (new renderer% [tree tree]))
  (define id (get-field id r))
  (define root (send r render (and parent (renderer-root parent))))
  (log-gui-easy-debug "rendered window ~a" id)
  (begin0 r
    (hash-set! renderers id r)
    (send root show #t)
    (when (send tree is-dialog?)
      (send r destroy)
      (log-gui-easy-debug "destroyed window ~a" id))))

(define (render-popup-menu r tree x y)
  (define id (next-id!))
  (define menu-r (new renderer% [tree tree]))
  (define menu (send menu-r render #f))
  (define window (send r get-root))
  (log-gui-easy-debug "rendered popup menu ~a" id)
  (send window popup-menu menu x y)
  (send menu-r destroy)
  (log-gui-easy-debug "destroyed popup menu ~a" id))

(define (renderer-root r)
  (send r get-root))
