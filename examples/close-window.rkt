#lang racket/base

(require racket/class
         racket/gui/easy
         racket/gui/easy/operator)

(define/obs @visible? #t)
(render
 (window
  #:mixin (λ (%)
            (class %
              (super-new)
              (obs-observe!
               @visible?
               (λ (visible?)
                 (send this show visible?)))))
  (button
   "Hide temporarily..."
   (λ ()
     (@visible? . := . #f)
     (thread
      (λ ()
        (sleep 5)
        (@visible? . := . #t)))))))
