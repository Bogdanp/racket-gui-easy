#lang racket/base

(require (prefix-in gui: racket/gui)
         racket/gui/easy
         racket/gui/easy/operator
         racket/gui/easy/private/view/image)

(define @path (@ #f))

(render
 (window
  (button "Choose image..."
          (λ ()
            (define path (gui:get-file))
            (when path (@path . := . path))))
  (cond/view
   [@path (image @path)]
   [else (text "Please choose an image to display.")])))
