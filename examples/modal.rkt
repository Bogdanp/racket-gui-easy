#lang racket/base

(require racket/gui/easy
         racket/gui/easy/operator)

(define @msg (@ "Click the button"))

(define r
  (render
   (window
    (vpanel
     (text @msg)
     (button
      "Display Modal"
      (λ ()
        (render
         (dialog
          #:style '(close-button)
          (input @msg (λ (_ text)
                        (@msg . := . text))))
         (renderer-root r))))))))
