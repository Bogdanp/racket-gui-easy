#lang racket/base

(require racket/format
         racket/gui/easy
         racket/gui/easy/operator)

(define (F->C f) (* (- f 32) 5/9))
(define (C->F c) (+ (* c 9/5) 32))

(define @background (@ #f))
(define @tempC (@ 26))
(define @tempF (@tempC . ~> . C->F))

(define (temp text @value [convert values])
  (hpanel
   (label text)
   (input
    #:font (font "Operator Mono" 12 #:family 'modern)
    #:background-color @background
    (@value . ~> . ~r)
    (λ (_event text)
      (cond
        [(string->number text)
         => (λ (c)
              (@tempC . := . (convert c))
              (@background . := . #f))]

        [else
         (@background . := . (color "red"))])))))

(render
 (window
  #:title "Temperature Converter"
  #:size (cons 200 100)
  (vpanel
   (temp "Celsius: "    @tempC     )
   (temp "Fahrenheit: " @tempF F->C))))
