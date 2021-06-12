#lang racket/base

(require "observable.rkt")

(provide @ := λ:= <~ λ<~ ~>)

(define (@ v)
  (if (obs? v) v (obs v)))

(define (:= o v)
  (o . <~ . (λ (_) v)))

(define ((λ:= o [f values]) v)
  (o . := . (f v)))

(define (<~ o f)
  (obs-update! o f))

(define (~> o f)
  (obs-map o f))

(define (λ<~ o f)
  (λ () (<~ o f)))
