#lang racket/base

(require racket/gui/easy
         racket/gui/easy/operator)

(define/obs @count 0)

(render
 (window
  #:title "Counter"
  (hpanel
   (input
    #:enabled? #f
    #:min-size '(200 #f)
    (@count . ~> . number->string))
   (button
    "Count"
    (@count . λ<~ . add1)))))
