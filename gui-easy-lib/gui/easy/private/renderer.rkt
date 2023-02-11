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

 current-renderer
 renderer<%>
 renderer-root
 renderer-destroy)

(define current-renderer
  (make-parameter #f))

(struct dependency-set (deps procs))

(define id-seq (box 0))
(define update-id-seq! (make-box-update-proc id-seq))
(define (next-id!)
  (update-id-seq! add1))

(define renderers (make-hasheqv))
(define renderer<%>
  (interface ()
    get-root
    render
    add-dependencies
    remove-dependencies
    destroy))

(define renderer%
  (class* object% (renderer<%>)
    (init-field tree)
    (field [id (next-id!)])
    (super-new)

    (define depss null)
    (define root #f)
    (define/public (get-root) root)

    (define/public (render parent)
      (parameterize ([current-renderer this])
        (set! root (send tree create parent)))
      (begin0 root
        (do-add-dependencies (send tree dependencies) tree root)))

    (define/public (add-dependencies deps tree root)
      (do-add-dependencies deps tree root))

    (define/public (remove-dependencies s)
      (do-remove-dependencies s))

    (define/public (destroy)
      (gui:queue-callback
       (lambda ()
         (hash-remove! renderers id)
         (when root
           (for-each do-remove-dependencies depss)
           (parameterize ([current-renderer this])
             (send tree destroy root))
           (set! root #f)))))

    (define (do-add-dependencies deps tree root)
      (define s
        (dependency-set deps (for/list ([dep (in-list deps)])
                               (define (proc v)
                                 (gui:queue-callback
                                  (lambda ()
                                    (parameterize ([current-renderer this])
                                      (send tree update root dep v)))))
                               (begin0 proc
                                 (obs-observe! dep proc)))))
      (begin0 s
        (set! depss (cons s depss))))

    (define (do-remove-dependencies s)
      (for ([dep (in-list (dependency-set-deps s))]
            [proc (in-list (dependency-set-procs s))])
        (obs-unobserve! dep proc))
      (set! depss (remq s depss)))))

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

(define (render-popup-menu parent tree x y)
  (define r (new renderer% [tree tree]))
  (define id (get-field id r))
  (define menu (send r render #f))
  (define window (send parent get-root))
  (log-gui-easy-debug "rendered popup menu ~a" id)
  (send window popup-menu menu x y)
  (send r destroy)
  (log-gui-easy-debug "destroyed popup menu ~a" id))

(define (renderer-root r)
  (send r get-root))

(define (renderer-destroy r)
  (send r destroy))
