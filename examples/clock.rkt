#lang racket/base

(require plot
         racket/class
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

(define @show-graph? (obs #f))

(define r 90)
(define red (color "red"))
(define black (color "black"))

(define (ang v [p 30])
  (+ pi (* v (- (/ pi p)))))

(define (draw-hand dc l w c a)
  (send dc set-pen c w 'solid)
  (send dc draw-line
        r r
        (+ r (* l (sin a)))
        (+ r (* l (cos a)))))

(define (overlay s)
  (list
   (vrule s #:color 0)
   (point-label
    (list s 0)
    (format "(~a, ~a)"
            (round (+ r (* r (sin (ang s 30)))))
            (round (+ r (* r (cos (ang s 30)))))))))

(render
 (window
  #:title "Clock"
  (vpanel
   (hpanel
    #:alignment '(center center)
    (hpanel
     #:stretch '(#f #f)
     #:min-size `(,(+ 10 (* 2 r))
                  ,(+ 10 (* 2 r)))
     (canvas
      @d
      #:style '(transparent)
      #:margin '(5 5)
      (λ (dc d)
        (define h (modulo (date-hour d) 12))
        (define m (date-minute d))
        (define s (date-second d))
        (send dc set-smoothing 'smoothed)
        (send dc set-pen black 1 'solid)
        (send dc draw-ellipse 0 0 (* 2 r) (* 2 r))
        (draw-hand dc (* r 0.80) 1 red   (ang s 30))
        (draw-hand dc (* r 0.65) 2 black (ang m 30))
        (draw-hand dc (* r 0.55) 3 black (ang h 6))))))
   (checkbox
    #:label "Show graph?"
    #:checked? @show-graph?
    (λ:= @show-graph?))
   (cond/view
    [@show-graph?
     (snip
      @d
      #:min-size '(400 200)
      (λ (d w h)
        (define the-snip
          (parameterize ([plot-width w]
                         [plot-height h]
                         [plot-x-label "Seconds"]
                         [plot-y-label "Position"])
            (plot-snip
             (list
              (lines
               (for/list ([s (in-range 0 60)])
                 (list s (sin (ang s 30)))))
              (lines
               (for/list ([s (in-range 0 60)])
                 (list s (cos (ang s 30)))))))))

        (begin0 the-snip
          (send the-snip set-overlay-renderers (overlay (date-second d)))))
      (λ (the-snip d)
        (send the-snip set-overlay-renderers (overlay (date-second d)))))]

    [else
     (hpanel)]))))
