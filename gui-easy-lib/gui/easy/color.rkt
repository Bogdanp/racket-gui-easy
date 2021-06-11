#lang racket/base

(require racket/class
         racket/contract
         (prefix-in gui: racket/gui))

(provide
 (contract-out
  [color (case->
          (-> string? (is-a?/c gui:color%))
          (-> byte? byte? byte? (is-a?/c gui:color%))
          (-> byte? byte? byte? (real-in 0 1.0) (is-a?/c gui:color%)))]))

(define (color . args)
  (apply make-object gui:color% args))
