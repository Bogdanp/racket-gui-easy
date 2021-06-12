#lang racket/base

(require racket/gui/easy
         racket/gui/easy/operator)

(define @n (@ 50))
(define @n-str (@n . ~> . number->string))

(render
 (window
  (vpanel
   (text @n-str)
   (slider
    #:style '(horizontal plain)
    @n (λ:= @n))
   (input @n-str (λ (_ text)
                   (define n (string->number text))
                   (when (and n (>= n 0) (<= n 100))
                     (@n . := . n)))))))
