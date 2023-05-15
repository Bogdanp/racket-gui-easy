#lang racket/base

(define-syntax-rule (reprovide mod ...)
  (begin
    (require mod ...)
    (provide (all-from-out mod ...))))

(reprovide "easy/color.rkt"
           "easy/font.rkt"
           "easy/observable.rkt"
           "easy/renderer.rkt"
           "easy/view.rkt")

(module reader syntax/module-reader
  racket/gui/easy/hash-lang)
