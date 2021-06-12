#lang racket/base

(require racket/date
         racket/gui/easy
         racket/gui/easy/operator)

(define @seconds
  (let ([@seconds (@ (current-seconds))])
    (begin0 @seconds
      (thread
       (lambda ()
         (let loop ()
           (@seconds . := . (current-seconds))
           (sleep 1)
           (loop)))))))

(define @c1 (@ 0))
(define @c2 (@ 0))
(define @title
  (obs-combine
   (λ (c1 c2 ts)
     (format "Counters: ~a ~a at ~a" c1 c2 (date->string (seconds->date ts) #t)))
   @c1 @c2 @seconds))

(define (counter @c)
  (hpanel
   (button "-" (@c . λ<~ . sub1))
   (text (@c . ~> . number->string))
   (button "+" (@c . λ<~ . add1))))

(render
 (window
  #:title @title
  (vpanel
   (counter @c1)
   (counter @c2))))
