#lang racket/base

(require racket/file
         (prefix-in gui: racket/gui)
         racket/gui/easy
         racket/gui/easy/operator)

(define @path (@ #f))
(define @width (@ 200))
(define @height (@ 200))
(define @size
  (obs-combine
   (λ (w h)
     (list
      (and (> w 0) w)
      (and (> h 0) h)))
   @width @height))
(define @mode (@ 'fit))

;; The contract on `image' requires the path to be a valid filesystem
;; path so, when there is no current image, we need to be able to
;; fall back to some path.
(define fallback-path (make-temporary-file))
(define (path-fallback p)
  (or p fallback-path))

(render
 (window
  #:size '(800 600)
  (button "Choose image..."
          (λ ()
            (define path (gui:get-file))
            (when path (@path . := . path))))
  (hpanel
   #:stretch '(#f #f)
   (cond-view
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
      (image (@path . ~> . path-fallback) #:size @size #:mode @mode))]
    [else
     (text "Please choose an image to display.")]))))
