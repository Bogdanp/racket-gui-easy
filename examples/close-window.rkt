#lang racket/base

(require racket/class
         (prefix-in gui: racket/gui)
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
     (new gui:timer%
          [interval 5000]
          [just-once? #t]
          [notify-callback (lambda ()
                             (@visible? . := . #t))])))))
