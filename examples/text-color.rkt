#lang racket/base

(require (prefix-in gui: racket/gui)
         racket/gui/easy
         racket/gui/easy/operator
         racket/random)

(define colors
  (list
   "red"
   "cornflowerblue"
   (gui:make-color #xFF #x00 #xFF 1.0)
   (gui:make-color #xFF #x00 #xFF 0.5)
   (gui:make-color #x00 #xFF #x00 1.0)
   (gui:make-color #x00 #x00 #xFF 1.0)))

(define @color (@ (random-ref colors)))

(render
 (window
  (text
   #:color @color
   "Hello, world!")
  (button
   "Change color"
   (λ ()
     (@color . := . (random-ref colors))))))
