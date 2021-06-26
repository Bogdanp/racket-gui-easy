#lang racket/base

(require "common.rkt"
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

(struct obs (value-box observers-box))

(define (->obs v)
  (cond
    [(obs? v) v]
    [else (make-obs v)]))

(define (make-obs v)
  (obs (box v) (box null)))

(define (obs-observe! o observer)
  (void
   (box-update
    (obs-observers-box o)
    (λ (obss)
      (cons observer obss)))))

(define (obs-unobserve! o observer)
  (void
   (box-update
    (obs-observers-box o)
    (λ (obss)
      (remq observer obss)))))

(define (obs-update! o f)
  (define v (box-update (obs-value-box o) f))
  (begin0 v
    (for ([obs (in-list (reverse (unbox (obs-observers-box o))))])
      (with-handlers ([exn:fail?
                       (lambda (e)
                         ((error-display-handler) (exn-message e) e))])
        (obs v)))))

(define (obs-peek o)
  (unbox (obs-value-box o)))

(define (obs-map a f)
  (define b (make-obs (f (obs-peek a))))
  (define b-box (make-weak-box b))
  (define (g v)
    (define maybe-b (weak-box-value b-box))
    (when maybe-b
      (define w (f v))
      (obs-update! maybe-b (λ (_) w))))
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
  (define b (make-obs (apply f (vector->list vals))))
  (define b-box (make-weak-box b))
  (define gs
    (for/list ([o (in-list os)]
               [i (in-naturals)])
      (define (g v)
        (define maybe-b (weak-box-value b-box))
        (when maybe-b
          (vector-set! vals i v)
          (define w (apply f (vector->list vals)))
          (obs-update! maybe-b (λ (_) w))))
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
  (define b (make-obs (obs-peek a)))
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
                 (obs-update! maybe-b (λ (_) pending)))
               (loop nothing))))))))
  (define (f v)
    (channel-put ch v))
  (begin0 b
    (obs-observe! a f)
    (will-register executor b (λ (_)
                                (log-gui-easy-debug "obs-debounce: unobserve ~.s" f)
                                (obs-unobserve! a f)
                                (channel-put ch stop)))))
