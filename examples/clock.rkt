#lang racket/base

(require racket/class
         racket/date
         racket/gui/easy
         racket/gui/easy/operator
         racket/math)

(define @d
  (let ([@d (obs (current-date))])
    (begin0 @d
      (thread
       (lambda ()
         (let loop ()
           (@d . := . (current-date))
           (sleep 1)
           (loop)))))))

(define r 90)
(define red (color "red"))
(define black (color "black"))

(define (draw-hand dc l w c ang)
  (send dc set-pen c w 'solid)
  (send dc draw-line
        r r
        (+ r (* l (sin ang)))
        (+ r (* l (cos ang)))))

(render
 (window
  #:title "Clock"
  #:size (list (* 2 r) (+ (* 2 r) 30))
  (canvas @d (λ (dc d)
               (define h (modulo (date-hour d) 12))
               (define m (date-minute d))
               (define s (date-second d))
               (send dc clear)
               (send dc set-smoothing 'smoothed)
               (send dc set-pen (color "black") 1 'solid)
               (send dc draw-ellipse 0 0 (* 2 r) (* 2 r))
               (draw-hand dc (* r 0.80) 1 red   (+ pi (* s (- (/ pi 30)))))
               (draw-hand dc (* r 0.65) 2 black (+ pi (* m (- (/ pi 30)))))
               (draw-hand dc (* r 0.55) 3 black (+ pi (* h (- (/ pi 6)))))))))
