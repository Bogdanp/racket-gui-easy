#lang racket/base

(require box-extra
         racket/class
         (prefix-in gui: racket/gui)
         "class.rkt"
         "logger.rkt"
         "observable.rkt")

(provide
 embed

 render
 render-popup-menu
 render-menu-bar

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
    (init-private-field tree)
    (super-new)

    (define depss null)
    (define id (next-id!))
    (define/public (get-id) id)
    (define root #f)
    (define/public (get-root) root)

    (define/public (render parent)
      (parameterize ([current-renderer this])
        (set! root (send tree create parent)))
      (begin0 root
        (do-add-dependencies (send tree dependencies) tree root)))

    (define/public (add-dependencies deps tree root) ;; noqa
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
           (set! root #f)))
       #;high-priority? #t))

    (define (do-add-dependencies deps tree root) ;; noqa
      (define s
        (dependency-set deps (for/list ([dep (in-list deps)])
                               (define (proc v)
                                 (gui:queue-callback
                                  (lambda ()
                                    (parameterize ([current-renderer this])
                                      (send tree update root dep v)))
                                  #;high-priority? #f))
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
  (define id (send r get-id))
  (send r render parent)
  (log-gui-easy-debug "rendered tree ~a" id)
  (begin0 r
    (hash-set! renderers id r)))

(define (render tree
                [parent #f]
                #:wait? [wait? (send tree is-dialog?)])
  (define r (new renderer% [tree tree]))
  (define id (send r get-id))
  (define root (send r render (and parent (renderer-root parent))))
  (log-gui-easy-debug "rendered window ~a" id)
  (hash-set! renderers id r)
  (cond
    [(send tree is-dialog?)
     (if wait?
         (send root show #t)
         (send root show-without-yield))]
    [else
     (if wait?
         (send root show-with-yield)
         (send root show #t))])
  (cond
    [wait?
     (send r destroy)
     (log-gui-easy-debug "destroyed window ~a" id)]
    [else r]))

(define (render-popup-menu parent tree x y #:wait? [wait? #t])
  (define r (new renderer% [tree tree]))
  (define id (send r get-id))
  (define done (make-semaphore))
  (define menu (send r render (lambda () (semaphore-post done))))
  (define window (send parent get-root))
  (log-gui-easy-debug "rendered popup menu ~a" id)
  (send window popup-menu menu x y)
  (cond
    [wait?
     (gui:yield done)
     (send r destroy)
     (log-gui-easy-debug "destroyed popup menu ~a" id)]
    [else
     r]))

(define (render-menu-bar tree)
  (define r (new renderer% [tree tree]))
  (define id (send r get-id))
  (send r render 'root)
  (log-gui-easy-debug "rendered root menu bar ~a" id)
  r)

(define (renderer-root r)
  (send r get-root))

(define (renderer-destroy r)
  (send r destroy))
