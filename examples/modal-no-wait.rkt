#lang racket/gui/easy

(define (make-dialog close)
  (dialog
   #:title "A Dialog"
   (button "Close Me" close)))

(define root
  (render
   (window
    (button
     "Open Dialog"
     (lambda ()
       (define d (make-dialog (λ () (renderer-destroy child))))
       (define child (render d root #:wait? #f))
       (displayln "opened dialog")
       (void))))))
