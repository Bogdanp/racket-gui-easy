#lang racket/base

(require racket/class
         racket/gui/easy
         racket/gui/easy/operator)

(define @pos (@ '(0 0)))

(render
 (window
  #:mixin (λ (%)
            (class % (super-new)
              (define/override (on-move x y)
                (@pos . := . `(,x ,y)))))
  (text (@pos . ~> . (λ (pos)
                       (apply format "Current position: (~s, ~s)" pos))))))
