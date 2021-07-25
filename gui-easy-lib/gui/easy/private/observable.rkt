#lang racket/base

(require box-extra
         "executor.rkt"
         "logger.rkt")

(provide
 (rename-out [make-obs obs])
 ->obs
 obs?
 obs-observe!
 obs-unobserve!
 obs-update!
 obs-peek
 obs-map
 obs-combine
 obs-debounce)

(struct obs
  (value-box
   update-value-box!
   observers-box
   update-observers-box!
   mapped?))

(define (->obs v)
  (cond
    [(obs? v) v]
    [else (make-obs v)]))

(define (make-obs v [mapped? #f])
  (define value-box (box v))
  (define observers-box (box null))
  (define update-value-box! (make-box-update-proc value-box))
  (define update-observers-box! (make-box-update-proc observers-box))
  (obs value-box
       update-value-box!
       observers-box
       update-observers-box!
       mapped?))

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
  (define v ((obs-update-value-box! o) f))
  (begin0 v
    (for ([obs (in-list (reverse (unbox (obs-observers-box o))))])
      (with-handlers ([exn:fail?
                       (lambda (e)
                         ((error-display-handler) (exn-message e) e))])
        (obs v)))))

(define (obs-update! o f)
  (when (obs-mapped? o)
    (raise-argument-error 'obs-update! "an unmapped observable" o))
  (do-obs-update! o f))

(define (obs-peek o)
  (unbox (obs-value-box o)))

(define (obs-map a f)
  (define b (make-obs (f (obs-peek a)) #t))
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

(define (obs-combine f . os)
  (define vals
    (for/vector #:length (length os)
        ([o (in-list os)])
      (obs-peek o)))
  (define b (make-obs (apply f (vector->list vals)) #t))
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
  (define b (make-obs (obs-peek a) #t))
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

(module+ test
  (require rackunit)

  (define @a (make-obs 1))
  (check-equal? (obs-peek @a) 1)
  (check-equal? (obs-update! @a add1) 2)
  (check-equal? (obs-peek @a) 2)

  (define @b (obs-map @a number->string))
  (check-equal? (obs-peek @b) "2")
  (check-exn
   #rx"an unmapped observable"
   (λ () (obs-update! @b "3")))
  (check-equal? (obs-peek @b) "2")
  (check-equal? (obs-update! @a add1) 3)
  (check-equal? (obs-peek @b) "3"))
