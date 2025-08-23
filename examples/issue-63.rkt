#lang racket/base

(require racket/gui/easy
         racket/gui/easy/debugger
         racket/gui/easy/operator)

(define @a (@ 0))
(define @b (@ 0))
(define @b2 (obs-throttle @b))

(module+ main
  (start-debugger)
  (render
   (window
    (hpanel
     (text "Slider on @a")
     (slider @a #:min-value 0 #:max-value 100
             (λ (v) (:= @a v)))
     (text (obs-map @a number->string)))

    (hpanel
     (text "Slider on @b")
     (slider @b #:min-value 0 #:max-value 100
             (λ (v) (:= @b v)))
     (text (obs-map @b number->string))
     (text (obs-map @b2 number->string)))

    (hpanel
     (text "Slider on throttled @b")
     (slider @b2 #:min-value 0 #:max-value 100
             (λ (v) (:= @b v)))
     (text (obs-map @b number->string))
     (text (obs-map @b2 number->string))))))
