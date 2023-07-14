#lang racket/gui/easy
(define/obs @counter 0)
(define/obs @evens (~#> @counter even?))
(render
 (window
  (hpanel
   (button "-" (λ () (@counter . <~ . sub1)))
   (text
    (obs-combine
     (λ (c e)
       (format "~a (~a)" e c))
     @counter @evens))
   (button "+" (λ () (@counter . <~ . add1))))))
