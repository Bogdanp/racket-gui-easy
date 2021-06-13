#lang racket/base

(require (prefix-in gui: racket/gui)
         racket/gui/easy
         racket/gui/easy/operator)

(define @path (@ #f))
(define @width (@ 200))
(define @height (@ 200))
(define @size (obs-combine list @width @height))
(define @mode (@ 'fit))

(render
 (window
  #:size '(800 600)
  (button "Choose image..."
          (λ ()
            (define path (gui:get-file))
            (when path (@path . := . path))))
  (hpanel
   #:stretch '(#f #f)
   (cond/view
    [@path
     (vpanel
      (vpanel
       #:stretch '(#f #f)
       (choice
        '("Fit" "Fill")
        #:selection (@mode . ~> . (compose1 string-titlecase symbol->string))
        (@mode . λ:= . (compose1 string->symbol string-downcase)))
       (slider
        #:label "Width"
        #:min-value 0
        #:max-value 800
        @width (λ:= @width))
       (slider
        #:label "Height"
        #:min-value 0
        #:max-value 800
        @height (λ:= @height)))
      (image @path #:size @size #:mode @mode))]
    [else
     (text "Please choose an image to display.")]))))
