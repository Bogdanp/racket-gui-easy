#lang racket/base

(provide
 box-update)

(define (box-update b f)
  (let loop ([v (unbox b)])
    (define w (f v))
    (cond
      [(box-cas! b v w) w]
      [else (loop (unbox b))])))
