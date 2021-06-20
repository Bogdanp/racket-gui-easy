#lang racket/base

(require (for-syntax racket/base))

(define-syntax-rule (reprovide mod ...)
  (begin
    (require mod ...)
    (provide (all-from-out mod ...))))

(reprovide "view/button.rkt"
           "view/canvas.rkt"
           "view/checkbox.rkt"
           "view/choice.rkt"
           "view/if.rkt"
           "view/image.rkt"
           "view/input.rkt"
           "view/panel.rkt"
           "view/progress.rkt"
           "view/slider.rkt"
           "view/snip.rkt"
           "view/spacer.rkt"
           "view/table.rkt"
           "view/tabs.rkt"
           "view/text.rkt"
           "view/view.rkt"
           "view/window.rkt")
