#lang racket/base

(require (for-syntax racket/base))

(define-syntax-rule (reprovide mod ...)
  (begin
    (require mod ...)
    (provide (all-from-out mod ...))))

(reprovide "view/button.rkt"
           "view/checkbox.rkt"
           "view/choice.rkt"
           "view/if.rkt"
           "view/input.rkt"
           "view/panel.rkt"
           "view/progress.rkt"
           "view/slider.rkt"
           "view/spacer.rkt"
           "view/text.rkt"
           "view/view.rkt"
           "view/window.rkt")
