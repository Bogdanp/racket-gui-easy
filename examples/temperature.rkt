#lang racket/base

(require racket/format
         racket/gui/easy
         racket/gui/easy/operator)

(define (F->C f) (* (- f 32) 5/9))
(define (C->F c) (+ (* c 9/5) 32))

(define @tempC (@ 26))
(define @tempF (@tempC . ~> . C->F))

(define @background (@ #f))

(define input-font
  (font "Operator Mono" 12 #:family 'modern))

(render
 (window
  #:label "Temperature Converter"
  #:size (cons 200 100)
  (vpanel
   (hpanel
    (label "Celsius: ")
    (input
     #:font input-font
     #:background-color @background
     (@tempC . ~> . ~r)
     (λ (_event text)
       (cond
         [(string->number text)
          => (λ (c)
               (@tempC . := . c)
               (@background . := . #f))]

         [else
          (@background . := . (color "red"))]))))
   (hpanel
    (label "Fahrenheit: ")
    (input
     #:font input-font
     #:background-color @background
     (@tempF . ~> . ~r)
     (λ (_event text)
       (cond
         [(string->number text)
          => (λ (f)
               (@tempC . := . (F->C f))
               (@background . := . #f))]

         [else
          (@background . := . (color "red"))])))))))
