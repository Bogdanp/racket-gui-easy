#lang racket/base

(require racket/format
         racket/gui/easy
         racket/gui/easy/operator)

(define (F->C f) (* (- f 32) 5/9))
(define (C->F c) (+ (* c 9/5) 32))

(define @tempC (@ 26))
(define @tempF (@tempC . ~> . C->F))

(render
 (window
  #:label "Temperature Converter"
  #:size (cons 200 100)
  (vpanel
   (hpanel
    (label "Celsius: ")
    (input (@tempC . ~> . ~r)
           (λ (_event text)
             (define c (string->number text))
             (when c (@tempC . := . c)))))
   (hpanel
    (label "Fahrenheit: ")
    (input (@tempF . ~> . ~r)
           (λ (_event text)
             (define f (string->number text))
             (when f (@tempC . := . (F->C f)))))))))
