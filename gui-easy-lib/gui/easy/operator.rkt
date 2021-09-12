#lang racket/base

(require (for-syntax racket/base)
         syntax/parse/define
         "observable.rkt")

(provide define/obs @ := λ:= <~ λ<~ ~>)

(define-syntax-parser define/obs
  [(_ name:id v:expr)
   #'(define name
       (let ([e v])
         (if (obs? e)
             (begin0 e
               (obs-rename! e 'name))
             (obs e #:name 'name))))])

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
