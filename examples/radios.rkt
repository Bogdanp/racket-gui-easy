#lang racket/base

(require racket/gui/easy
         racket/gui/easy/operator)

(define @selection (@ 2))

(render
 (window
  (vpanel
   (radios
    '(1 2 3 4)
    #:choice->label (λ (v) (number->string (* v v)))
    #:choice=? eqv?
    #:selection @selection
    (λ (selection)
      (@selection . := . selection)))
   (button
    "Clear selection"
    (λ ()
      (@selection . := . #f))))))
