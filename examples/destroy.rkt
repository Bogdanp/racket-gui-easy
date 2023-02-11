#lang racket/base

(require racket/gui/easy)

(define r
  (render
   (window
    (button
     "Close"
     (λ ()
       (renderer-destroy r))))))
