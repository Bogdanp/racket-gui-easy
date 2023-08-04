#lang racket/gui/easy

(require racket/class
         racket/match)

(struct circle (x y))

(define/obs @circles null)

(define radius 50)
(define radius/2 (quotient radius 2))

(define (make-circle x y)
  (circle
   (- x radius/2)
   (- y radius/2)))

(define (distance c1 c2)
  (match-define (circle c1x c1y) c1)
  (match-define (circle c2x c2y) c2)
  (define dx (- c1x c2x))
  (define dy (- c1y c2y))
  (sqrt (+ (* dx dx)
           (* dy dy))))

(define (find-circle x y)
  (define c* (make-circle x y))
  (for/first ([c (in-list (reverse (obs-peek @circles)))]
              #:when (<= (distance c* c) radius/2))
    c))

(define (add-circle x y)
  (@circles . <~ . (λ (cs) (cons (make-circle x y) cs))))

(define (remove-circle c)
  (@circles . <~ . (λ (cs) (remq c cs))))

(define (clear-cirlces)
  (@circles . := . null))

(define the-renderer
  (render
   (window
    #:title "Popup Menu"
    #:size '(800 600)
    (vpanel
     (button
      "Clear"
      (λ () (clear-cirlces)))
     (canvas
      #:mixin (λ (%)
                (class %
                  (super-new)
                  (define/override (on-event e)
                    (case (send e get-event-type)
                      [(left-up)
                       (define x (send e get-x))
                       (define y (send e get-y))
                       (unless (find-circle x y)
                         (add-circle
                          (send e get-x)
                          (send e get-y)))]
                      [(right-up)
                       (define x (send e get-x))
                       (define y (send e get-y))
                       (define-values (window-x window-y)
                         (let loop ([x x]
                                    [y y]
                                    [v this])
                           (define parent (send v get-parent))
                           (if parent
                               (loop (+ x (send v get-x))
                                     (+ y (send v get-y))
                                     parent)
                               (values x y))))
                       (define c (find-circle x y))
                       (when c
                         (render-popup-menu
                          the-renderer
                          (popup-menu
                           (menu-item "Delete" (λ () (remove-circle c))))
                          window-x window-y))]))))
      @circles
      (λ (dc circles)
        (send dc clear)
        (for ([c (in-list circles)])
          (send dc draw-ellipse (circle-x c) (circle-y c) radius radius))))))))
