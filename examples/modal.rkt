#lang racket/base

(require racket/gui/easy)

(define r
  (render
   (window
    (button
     "Display Modal"
     (λ ()
       (render
        (dialog
         #:style '(close-button)
         (label "Hi"))
        (renderer-root r)))))))
