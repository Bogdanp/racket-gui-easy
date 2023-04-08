#lang racket/base

(require racket/class
         racket/draw
         racket/gui/easy
         racket/gui/easy/operator
         racket/random)

(define/obs @color "red")
(define/obs @stretch-h? #t)
(define/obs @stretch-v? #t)

(thread
 (lambda ()
   (let loop ()
     (@color . := . (random-ref '("red" "green" "blue")))
     (sleep 1)
     (loop))))

(render
 (window
  #:size '(800 600)
  (vpanel
   (hpanel
    #:stretch '(#t #f)
    (checkbox
     #:label "Stretch horizontally?"
     #:checked? @stretch-h?
     (λ:= @stretch-h?))
    (checkbox
     #:label "Stretch vertically?"
     #:checked? @stretch-v?
     (λ:= @stretch-v?)))
   ;; This wrap is normally unnecessary, but the point of this example
   ;; is to test that observable-view updates its area<%> to track its
   ;; child.
   (observable-view
    @color
    (λ (color)
      (canvas
       color
       #:min-size '(100 100)
       #:stretch (obs-combine list @stretch-h? @stretch-v?)
       (lambda (dc color)
         (define-values (w h)
           (send dc get-size))
         (send dc set-pen (new pen% [style 'transparent]))
         (send dc set-brush (new brush% [color color]))
         (send dc draw-rectangle 0 0 w h))))))))
