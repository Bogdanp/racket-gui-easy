#lang racket/base

(require racket/format
         racket/gui/easy
         racket/gui/easy/operator)

(define (F->C f) (* (- f 32) 5/9))
(define (C->F c) (+ (* c 9/5) 32))

(define/obs @background #f)
(define/obs @tempC 26)
(define/obs @tempF (@tempC . ~> . C->F))

(define (temp label @value [convert values])
  (hpanel
   (input
    #:font (font "Operator Mono" 12 #:family 'modern)
    #:background-color @background
    #:min-size '(80 #f)
    (@value . ~> . ~r)
    (λ (_event text)
      (cond
        [(string->number text)
         => (λ (c)
              (@tempC . := . (convert c))
              (@background . := . #f))]

        [else
         (@background . := . (color "red"))])))
   (text label)))

(render
 (window
  #:title "Temperature Converter"
  #:size '(300 #f)
  (hpanel
   (temp "Celsius"    @tempC     )
   (text " = ")
   (temp "Fahrenheit" @tempF F->C))))
