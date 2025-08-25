#lang racket/base

(require (for-syntax racket/base)
         box-extra
         racket/struct
         version-case
         "executor.rkt"
         "logger.rkt")

(provide
 (rename-out [make-obs obs])
 impersonate-obs
 chaperone-obs
 ->obs
 obs?
 obs-name
 obs-rename
 obs-derived?
 obs-observe!
 obs-unobserve!
 obs-update!
 obs-set!
 obs-peek
 obs-map
 obs-filter
 obs-filter-map
 obs-combine
 obs-debounce
 obs-throttle)

;; We don't guarantee it anywhere, but values are dispatched to
;; observers in subscription order. In recent versions of Racket, we can
;; faciliate this by using treelists. In older versions, we use regular
;; lists and append new observers to the end of the list, assuming that
;; any given observable won't enough observers to cause problems.
;;
;; Why not just keep a list and always reverse on update? Any given
;; observable is probably going to be updated more than it is going to
;; be subscribed to.
(version-case
 [(version>= (version) "8.12.0.7")
  (require racket/treelist)
  (define (make-observers)
    (treelist))
  (define (add-observer entries observer)
    (treelist-add entries observer))
  (define (remove-observer entries observer)
    (for/treelist ([v (in-treelist entries)]
                   #:unless (eq? v observer))
      v))
  (define-syntax in-observers
    (make-rename-transformer #'in-treelist))]
 [else
  (define (make-observers) ;; noqa
    null)
  (define (add-observer entries observer) ;; noqa
    (append entries (list observer)))
  (define (remove-observer entries observer) ;; noqa
    (remq observer entries))
  (define-syntax in-observers
    (make-rename-transformer #'in-list))])

(struct obs
  ([name #:mutable]
   handle
   [value-box #:mutable] ; mutable only to allow impersonation on ref
   [update-value-box! #:mutable]
   observers-box
   update-observers-box!
   derived?)

  #:property prop:object-name
  (λ (this) (obs-name this))

  #:methods gen:equal+hash
  [(define (equal-proc o1 o2 _recursive-equal?)
     (and (obs? o1)
          (obs? o2)
          (eq? (obs-handle o1)
               (obs-handle o2))))

   (define (hash-proc o recursive-equal-hash)
     (recursive-equal-hash (obs-handle o)))

   (define (hash2-proc o recursive-equal-hash)
     (recursive-equal-hash (obs-handle o)))]

  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (_) 'obs)
      (λ (o) (list
              (unbox (obs-value-box o))
              (unquoted-printing-string "#:name")
              (obs-name o)
              (unquoted-printing-string "#:derived?")
              (obs-derived? o)))))])

(define (->obs v)
  (cond
    [(obs? v) v]
    [else (make-obs v)]))

(define (make-obs v
                  #:name [name 'anon]
                  #:derived? [derived? #f])
  (define handle (gensym name))
  (define value-box (box v))
  (define observers-box (box (make-observers)))
  (define update-value-box! (make-box-update-proc value-box))
  (define update-observers-box! (make-box-update-proc observers-box))
  (obs name
       handle
       value-box
       update-value-box!
       observers-box
       update-observers-box!
       derived?))

(define (do-impersonate-obs o
                            do-impersonate-box
                            do-impersonate-procedure
                            val-ref
                            val-set)
  (impersonate-struct o
                      obs-value-box
                      (and val-ref
                           (lambda (o bx) ;; noqa
                             (do-impersonate-box
                              bx
                              (lambda (_ v) (val-ref o v))
                              (lambda (_ v) v))))
                      set-obs-value-box!
                      #f ; witness
                      obs-update-value-box!
                      (and val-set
                           (λ (o update-proc) ; update-proc: (a -> b) -> b ;; noqa
                             (do-impersonate-procedure
                              update-proc ; update-proc': (a -> b) -> c
                              (λ (proc) ; proc: a -> b
                                (λ (v) ; proc': a -> c
                                  ; val-set: b -> c
                                  (val-set o (proc v)))))))
                      set-obs-update-value-box!!
                      #f))

(define (chaperone-obs o
                       #:ref [val-ref #f]
                       #:set [val-set #f])
  (do-impersonate-obs o
                      chaperone-box
                      chaperone-procedure
                      val-ref
                      val-set))

(define (impersonate-obs o
                         #:ref [val-ref #f]
                         #:set [val-set #f])
  (do-impersonate-obs o
                      impersonate-box
                      impersonate-procedure
                      val-ref
                      val-set))

(define (obs-rename o name)
  (impersonate-struct o
                      obs-name
                      (λ (_ _name) name)
                      set-obs-name!
                      #f))

(define (obs-observe! o observer)
  (void
   ((obs-update-observers-box! o)
    (lambda (obss)
      (add-observer obss observer)))))

(define (obs-unobserve! o observer)
  (void
   ((obs-update-observers-box! o)
    (lambda (obss)
      (remove-observer obss observer)))))

(define (do-obs-update! o proc)
  (define old-v (unbox (obs-value-box o)))
  (define updated-v ((obs-update-value-box! o) proc))
  (log-change o old-v updated-v)
  (define obss (unbox (obs-observers-box o)))
  (for ([obs (in-observers obss)])
    (with-handlers ([exn:fail?
                     (lambda (e)
                       ((error-display-handler)
                        (format "do-obs-update!: ~a" (exn-message e))
                        e))])
      (obs updated-v)))
  updated-v)

(define (obs-update! o f)
  (when (obs-derived? o)
    (raise-argument-error 'obs-update! "(not/c obs-derived?)" o))
  (do-obs-update! o f))

(define (obs-set! o v)
  (void (obs-update! o (λ (_) v))))

(define (obs-peek o)
  (unbox (obs-value-box o)))

(define (obs-map a f)
  (define b (make-obs (f (obs-peek a)) #:derived? #t))
  (define b-box (make-weak-box b))
  (define (g v)
    (define maybe-b (weak-box-value b-box))
    (when maybe-b
      (define w (f v))
      (do-obs-update! maybe-b (λ (_) w))))
  (obs-observe! a g)
  (will-register
   executor b
   (lambda (_)
     (log-gui-easy-debug "obs-map: unobserve ~.s" f)
     (obs-unobserve! a g)))
  b)

(define (obs-filter a f [d #f])
  (obs-filter-map a (λ (v) (and (f v) v)) d))

(define (obs-filter-map a f [d #f])
  (define b
    (make-obs
     #:derived? #t
     (or (f (obs-peek a)) d)))
  (define b-box (make-weak-box b))
  (define (g v)
    (define w (f v))
    (when w
      (define maybe-b
        (weak-box-value b-box))
      (when maybe-b
        (do-obs-update! maybe-b (λ (_) w)))))
  (obs-observe! a g)
  (will-register
   executor b
   (lambda (_)
     (log-gui-easy-debug "obs-filter: unobserved ~.s" f)
     (obs-unobserve! a g)))
  b)

(define (obs-combine f . os)
  (define vals
    (for/vector
        #:length (length os)
        ([o (in-list os)])
      (obs-peek o)))
  (define b (make-obs (apply f (vector->list vals)) #:derived? #t))
  (define b-box (make-weak-box b))
  (define gs
    (for/list ([o (in-list os)]
               [i (in-naturals)])
      (define (g v)
        (define maybe-b (weak-box-value b-box))
        (when maybe-b
          (vector-set! vals i v)
          (define w (apply f (vector->list vals)))
          (do-obs-update! maybe-b (λ (_) w))))
      (obs-observe! o g)
      g))
  (will-register
   executor b
   (lambda (_)
     (log-gui-easy-debug "obs-combine: unobserve ~.s" f)
     (for ([o (in-list os)]
           [g (in-list gs)])
       (obs-unobserve! o g))))
  b)

(define nothing (gensym "nothing"))
(define stop (gensym "stop"))

(define (obs-debounce a #:duration [duration 200])
  (define b (make-obs (obs-peek a) #:derived? #t))
  (define b-box (make-weak-box b))
  (define thd
    (thread
     (lambda ()
       (let loop ([pending nothing])
         (sync
          (handle-evt
           (thread-receive-evt)
           (lambda (_)
             (define v (thread-receive))
             (unless (eq? v stop)
               (loop v))))
          (if (eq? pending nothing)
              never-evt
              (handle-evt
               (alarm-evt
                #;msecs (+ (current-inexact-monotonic-milliseconds) duration)
                #;monotonic? #t)
               (lambda (_)
                 (define maybe-b (weak-box-value b-box))
                 (when maybe-b
                   (do-obs-update! maybe-b (λ (_) pending)))
                 (loop nothing)))))))))
  (define (proc v)
    (thread-send thd v))
  (obs-observe! a proc)
  (will-register
   executor b
   (lambda (_)
     (log-gui-easy-debug "obs-debounce: unobserve ~.s" proc)
     (obs-unobserve! a proc)
     (thread-send thd stop)))
  b)

(define (obs-throttle a #:duration [duration 200])
  (define b (make-obs (obs-peek a) #:derived? #t))
  (define b-box (make-weak-box b))
  (define thd
    (thread
     (lambda ()
       (let loop ([pending nothing]
                  [pending-alarm #f])
         (sync
          (handle-evt
           (thread-receive-evt)
           (lambda (_)
             (define v (thread-receive))
             (unless (eq? v stop)
               (loop v (or pending-alarm
                           (alarm-evt
                            #;msecs (+ (current-inexact-monotonic-milliseconds) duration)
                            #;monotonic? #t))))))
          (if (eq? pending nothing)
              never-evt
              (handle-evt
               pending-alarm
               (lambda (_)
                 (define maybe-b (weak-box-value b-box))
                 (when maybe-b
                   (do-obs-update! maybe-b (λ (_) pending)))
                 (loop nothing #f)))))))))
  (define (proc v)
    (thread-send thd v))
  (obs-observe! a proc)
  (will-register
   executor b
   (lambda (_)
     (log-gui-easy-debug "obs-throttle: unobserve ~.s" proc)
     (obs-unobserve! a proc)
     (thread-send thd stop)))
  b)

(module+ test
  (require racket/port
           rackunit
           (submod ".."))

  (define @a (obs 1))
  (check-equal? (obs-peek @a) 1)
  (check-equal? (obs-update! @a add1) 2)
  (check-equal? (obs-peek @a) 2)

  (define @b (obs-map @a number->string))
  (check-equal? (obs-peek @b) "2")
  (check-exn
   #rx"not/c obs-derived"
   (λ () (obs-update! @b "3")))
  (check-equal? (obs-peek @b) "2")
  (check-equal? (obs-update! @a add1) 3)
  (check-equal? (obs-peek @b) "3")

  (define @c (obs 10))
  (define @d (obs-combine list @a @b @c))
  (check-equal? (obs-peek @d) (list 3 "3" 10))
  (obs-update! @a add1)
  (check-equal? (obs-peek @d) (list 4 "4" 10))
  (obs-update! @c add1)
  (check-equal? (obs-peek @d) (list 4 "4" 11))

  (define @evens (obs-filter @a even?))
  (define @odds (obs-filter @a odd?))
  (check-equal? (obs-peek @evens) 4)
  (check-equal? (obs-peek @odds) #f)
  (obs-update! @a add1)
  (check-equal? (obs-peek @evens) 4)
  (check-equal? (obs-peek @odds) 5)
  (obs-update! @a add1)
  (check-equal? (obs-peek @evens) 6)
  (check-equal? (obs-peek @odds) 5)
  (obs-update! @a add1)

  (test-case "observable names"
    (define @foo (obs 42 #:name '@foo))
    (check-equal? (object-name @foo) '@foo))

  (test-case "unobserve"
    (define x #f)
    (define (set-x! v)
      (set! x v))
    (define @obs (obs #f))
    (obs-observe! @obs set-x!)
    (obs-update! @obs (λ (_) 10))
    (sync (system-idle-evt))
    (check-equal? x 10)
    (obs-unobserve! @obs set-x!)
    (obs-update! @obs (λ (_) 42))
    (sync (system-idle-evt))
    (check-equal? x 10))

  (test-case "observer exn"
    (define x #f)
    (define y #f)
    (define @obs (obs #f))
    (obs-observe! @obs (λ (_) (error 'x "failed")))
    (obs-observe! @obs (λ (v) (set! y v)))
    (parameterize ([current-error-port (open-output-nowhere)])
      (obs-update! @obs (λ (_) 42)))
    (sync (system-idle-evt))
    (check-false x)
    (check-equal? y 42)))
