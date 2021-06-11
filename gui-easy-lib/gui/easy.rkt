#lang racket/base

(define-syntax-rule (reprovide mod ...)
  (begin
    (require mod ...)
    (provide (all-from-out mod ...))))

(reprovide "easy/observable.rkt"
           "easy/renderer.rkt"
           "easy/view.rkt")
