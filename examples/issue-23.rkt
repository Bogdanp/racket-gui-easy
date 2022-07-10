#lang racket/base

(require racket/format
         racket/gui/easy
         racket/gui/easy/operator)

(define/obs @on? #t)
(define/obs @seq 0)
(define/obs @timer 0)
(thread
 (lambda ()
   (let loop ()
     (sleep 1)
     (@timer . <~ . add1)
     (loop))))

(render
 (window
  #:title "Issue 23"
  #:size '(800 600)
  (vpanel
   (if-view @on?
            (dyn-view
             @seq
             (lambda (_)
               (text (@timer . ~> . ~a))))
            (text "Off"))
   (hpanel
    #:alignment '(center center)
    (button "Update" (λ () (@seq . <~ . add1)))
    (button "Toggle" (λ () (@on? . <~ . not)))))))
