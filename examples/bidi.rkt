#lang racket/base

(require racket/gui/easy
         racket/gui/easy/operator)

(define @msg (@ "Hi"))
(render
 (window
  #:title @msg
  (vpanel
   (input @msg (λ (_event text)
                 (@msg . := . text)))
   (label @msg))))
