#lang racket/base

(define-syntax-rule (reprovide mod ...)
  (begin
    (require mod ...)
    (provide (all-from-out mod ...))))

(reprovide "view/button.rkt"
           "view/canvas.rkt"
           "view/checkbox.rkt"
           "view/choice.rkt"
           "view/common.rkt"
           "view/hooks.rkt"
           "view/if.rkt"
           "view/image.rkt"
           "view/input.rkt"
           "view/list.rkt"
           "view/menu.rkt"
           "view/observable.rkt"
           "view/panel.rkt"
           "view/progress.rkt"
           "view/radios.rkt"
           "view/slider.rkt"
           "view/snip.rkt"
           "view/spacer.rkt"
           "view/table.rkt"
           "view/tabs.rkt"
           "view/text.rkt"
           "view/view.rkt"
           "view/window.rkt")
