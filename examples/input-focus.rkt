#lang racket/gui/easy

(define ((make-action who) event text)
  (println `(,who ,event ,text)))

(render
 (window
  (hpanel
   (text "A: ")
   (input "" (make-action 'A)))
  (hpanel
   (text "B: ")
   (input "" (make-action 'B)))))
