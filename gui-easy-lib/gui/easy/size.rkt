#lang racket/base

(require racket/contract
         (prefix-in gui: racket/gui)
         "private/view/size.rkt")

(provide
 fixed
 (contract-out
  (struct size
    ([w (or/c #f gui:dimension-integer?)]
     [h (or/c #f gui:dimension-integer?)]))
  (struct stretch
    ([w? boolean?]
     [h? boolean?]))))

(define fixed
  (stretch #f #f))
