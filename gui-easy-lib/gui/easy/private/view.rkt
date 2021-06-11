#lang racket/base

(require (for-syntax racket/base))

(define-syntax-rule (reprovide mod ...)
  (begin
    (require mod ...)
    (provide (all-from-out mod ...))))

(reprovide "view/button.rkt"
           "view/checkbox.rkt"
           "view/dialog.rkt"
           "view/if.rkt"
           "view/input.rkt"
           "view/label.rkt"
           "view/panel.rkt"
           "view/view.rkt"
           "view/window.rkt")
