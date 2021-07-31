#lang racket/base

(require racket/gui/easy
         racket/gui/easy/operator)

(define @msg (@ "Click the button"))

(define root-renderer
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
          (vpanel
           (text @msg)
           (input @msg (λ (_ text)
                         (@msg . := . text)))))
         root-renderer)))))))
