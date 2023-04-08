#lang racket/base

;; A port of rhombus-prototype/gui_demo.rhm to test propagation of
;; dyn-view dependencies up the tree.

(require racket/class
         racket/draw
         racket/gui/easy
         racket/gui/easy/operator
         racket/match)

(struct face (mood eyes?))

(define (draw-face dc f)
  (match-define (face mood eyes?) f)
  (define-values (w h)
    (send dc get-size))
  (define s (max 10 (- (min w h) 20)))
  (define x (/ (- w s) 2))
  (define y (/ (- h s) 2))
  (define π (atan 0 -1))

  (send* dc
    (set-pen (new pen% [style 'transparent]))
    (set-brush (new brush% [color "orange"]))
    (draw-ellipse x y s s))

  (send* dc
    (set-pen (new pen% [color "black"]))
    (set-brush (new brush% [style 'transparent])))
  (case mood
    [(happy)
     (send dc draw-arc
           (+ x (* 0.2 s))
           (+ y (* 0.2 s))
           (* 0.6 s)
           (* 0.6 s)
           (* π -3/4)
           (* π -1/4))]
    [else
     (send dc draw-arc
           (+ x (* 0.2 s))
           (+ y (* 0.7 s))
           (* 0.6 s)
           (* 0.6 s)
           (* π 1/4)
           (* π 3/4))])

  (when eyes?
    (send* dc
      (set-pen (new pen% [style 'transparent]))
      (set-brush (new brush% [color "black"]))
      (draw-ellipse
       (+ x (* 0.3 s))
       (+ y (* 0.3 s))
       (* 0.1 s)
       (* 0.1 s))
      (draw-ellipse
       (+ x (* 0.6 s))
       (+ y (* 0.3 s))
       (* 0.1 s)
       (* 0.1 s)))))

(define/obs @tab 'happy)
(define/obs @eyes? #t)

(define the-canvas
  (vpanel
   (canvas (obs-combine face @tab @eyes?) draw-face)
   (checkbox
    #:label "Eyes"
    #:checked? @eyes?
    (λ:= @eyes?))))

(render
 (window
  #:title "Face"
  #:size '(800 600)
  (tabs
   '(happy sad)
   #:choice->label (compose1 string-titlecase symbol->string)
   #:selection @tab
   (lambda (_what _choices selected)
     (@tab . := . selected))
   (dyn-view
    @tab
    (match-lambda
      ['happy the-canvas]
      [_
       (hpanel
        the-canvas
        (button "Be Happy" (λ () (@tab . := . 'happy))))])))))
