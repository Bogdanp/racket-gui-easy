#lang racket/base

(require racket/format
         racket/gui/easy
         racket/gui/easy/operator
         racket/math)

(define/obs @elapsed 0)
(define/obs @duration 10)

(thread
 (λ ()
   (let loop ()
     (@elapsed . <~ . (λ (e)
                        (define d (obs-peek @duration))
                        (define next-e (+ e 0.1))
                        (if (< next-e d) next-e d)))
     (sleep 0.1)
     (loop))))

(define (~seconds s)
  (~a (~r #:precision '(= 2) s) "s"))

(render
 (window
  #:title "Timer"
  (hpanel
   (text "Elapsed time:")
   (progress
    #:range (@duration . ~> . (λ (d) (* d 10)))
    (@elapsed . ~> . (λ (e) (exact-round (* e 10))))))
  (text (@elapsed . ~> . ~seconds))
  (hpanel
   (text "Duration:")
   (slider
    #:min-value 1
    #:max-value 100
    #:style '(horizontal plain)
    @duration
    (λ:= @duration)))
  (button
   "Reset"
   (λ ()
     (@elapsed . := . 0)))))
