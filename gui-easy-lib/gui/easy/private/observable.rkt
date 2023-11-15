#lang racket/base

(require box-extra
         racket/struct
         "executor.rkt"
         "logger.rkt")

(provide
 (rename-out [make-obs obs])
 impersonate-obs
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

(struct obs
  ([name #:mutable]
   handle
   value-box
   [update-value-box! #:mutable]
   observers-box
   update-observers-box!
   derived?)

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
  (define observers-box (box null))
  (define update-value-box! (make-box-update-proc value-box))
  (define update-observers-box! (make-box-update-proc observers-box))
  (obs name
       handle
       value-box
       update-value-box!
       observers-box
       update-observers-box!
       derived?))

(define (impersonate-obs o name-proc update-proc)
  (impersonate-struct o
                      obs-name
                      name-proc
                      set-obs-name!
                      (λ (_ name) name)
                      obs-update-value-box!
                      update-proc
                      set-obs-update-value-box!!
                      (λ (_ proc) proc)))

(define (obs-rename o name)
  (impersonate-obs o
                   (λ (_ _name) name)
                   (λ (_ update-proc) update-proc)))

(define (obs-observe! o observer)
  (void
   ((obs-update-observers-box! o)
    (λ (obss)
      (cons observer obss)))))

(define (obs-unobserve! o observer)
  (void
   ((obs-update-observers-box! o)
    (λ (obss)
      (remq observer obss)))))

(define (do-obs-update! o f)
  (define old-v (unbox (obs-value-box o)))
  (define v ((obs-update-value-box! o) f))
  (log-change o old-v v)
  (begin0 v
    (for ([obs (in-list (reverse (unbox (obs-observers-box o))))])
      (with-handlers ([exn:fail?
                       (lambda (e)
                         ((error-display-handler) (exn-message e) e))])
        (obs v)))))

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
  (begin0 b
    (obs-observe! a g)
    (will-register executor b (λ (_)
                                (log-gui-easy-debug "obs-map: unobserve ~.s" f)
                                (obs-unobserve! a g)))))

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
  (begin0 b
    (obs-observe! a g)
    (will-register executor b (λ (_)
                                (log-gui-easy-debug "obs-filter: unobserved ~.s" f)
                                (obs-unobserve! a g)))))

(define (obs-combine f . os)
  (define vals
    (for/vector #:length (length os)
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
      (begin0 g
        (obs-observe! o g))))
  (begin0 b
    (will-register executor b (λ (_)
                                (log-gui-easy-debug "obs-combine: unobserve ~.s" f)
                                (for ([o (in-list os)]
                                      [g (in-list gs)])
                                  (obs-unobserve! o g))))))

(define nothing (gensym "nothing"))
(define stop (gensym "stop"))

(define (obs-debounce a #:duration [duration 200])
  (define b (make-obs (obs-peek a) #:derived? #t))
  (define b-box (make-weak-box b))
  (define ch (make-channel))
  (thread
   (lambda ()
     (let loop ([pending nothing])
       (sync
        (handle-evt
         ch
         (lambda (v)
           (unless (eq? v stop)
             (loop v))))
        (if (eq? pending nothing)
            never-evt
            (handle-evt
             (alarm-evt (+ (current-inexact-milliseconds) duration))
             (lambda (_)
               (define maybe-b (weak-box-value b-box))
               (when maybe-b
                 (do-obs-update! maybe-b (λ (_) pending)))
               (loop nothing))))))))
  (define (f v)
    (channel-put ch v))
  (begin0 b
    (obs-observe! a f)
    (will-register executor b (λ (_)
                                (log-gui-easy-debug "obs-debounce: unobserve ~.s" f)
                                (obs-unobserve! a f)
                                (channel-put ch stop)))))

(define (obs-throttle a #:duration [duration 200])
  (define b (make-obs (obs-peek a) #:derived? #t))
  (define b-box (make-weak-box b))
  (define ch (make-channel))
  (thread
   (lambda ()
     (let loop ([pending nothing]
                [pending-alarm #f])
       (sync
        (handle-evt
         ch
         (lambda (v)
           (unless (eq? v stop)
             (loop v (or pending-alarm (alarm-evt (+ (current-inexact-milliseconds) duration)))))))
        (if (eq? pending nothing)
            never-evt
            (handle-evt
             pending-alarm
             (lambda (_)
               (define maybe-b (weak-box-value b-box))
               (when maybe-b
                 (do-obs-update! maybe-b (λ (_) pending)))
               (loop nothing #f))))))))
  (define (f v)
    (channel-put ch v))
  (begin0 b
    (obs-observe! a f)
    (will-register executor b (λ (_)
                                (log-gui-easy-debug "obs-throttle: unobserve ~.s" f)
                                (obs-unobserve! a f)
                                (channel-put ch stop)))))

(module+ test
  (require rackunit)

  (define @a (make-obs 1))
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

  (define @c (make-obs 10))
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
  (obs-update! @a add1))
