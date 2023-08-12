#lang racket/base

(require racket/class
         racket/contract
         racket/fixnum
         (prefix-in gui: racket/gui))

(provide
 (contract-out
  [color (case->
          (-> (or/c fixnum? string?) (is-a?/c gui:color%))
          (-> byte? byte? byte? (is-a?/c gui:color%))
          (-> byte? byte? byte? (real-in 0 1.0) (is-a?/c gui:color%)))]))

(define color
  (case-lambda
    [(color)
     (if (fixnum? color)
         (make-object gui:color%
                      (fxand (fxrshift color 24) #xFF)
                      (fxand (fxrshift color 16) #xFF)
                      (fxand (fxrshift color 8) #xFF)
                      (/ (fxand color #xFF) 255.0))
         (make-object gui:color% color))]
    [(r g b)
     (make-object gui:color% r g b)]
    [(r g b a)
     (make-object gui:color% r g b a)]))
