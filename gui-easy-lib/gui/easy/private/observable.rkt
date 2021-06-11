#lang racket/base

(require "common.rkt"
         "executor.rkt")

(provide
 (rename-out [make-obs obs])
 ->obs
 obs?
 obs-observe!
 obs-unobserve!
 obs-update!
 obs-peek
 obs-map
 obs-combine)

(struct obs (value-box observers-box))

(define (->obs v)
  (cond
    [(obs? v) v]
    [else (make-obs v)]))

(define (make-obs v)
  (obs (box v) (box null)))

(define (obs-observe! a observer)
  (void
   (box-update
    (obs-observers-box a)
    (λ (obss)
      (cons observer obss)))))

(define (obs-unobserve! a observer)
  (void
   (box-update
    (obs-observers-box a)
    (λ (obss)
      (remq observer obss)))))

(define (obs-update! a f)
  (define v (box-update (obs-value-box a) f))
  (begin0 v
    (for ([obs (in-list (unbox (obs-observers-box a)))])
      (with-handlers ([exn:fail?
                       (lambda (e)
                         ((error-display-handler) (exn-message e) e))])
        (obs v)))))

(define (obs-peek a)
  (unbox (obs-value-box a)))

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
                                (for ([o (in-list obs)]
                                      [g (in-list gs)])
                                  (obs-unobserve! o g))))))
