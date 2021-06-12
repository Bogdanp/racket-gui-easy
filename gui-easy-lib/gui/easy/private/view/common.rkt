#lang racket/base

(require (for-syntax racket/base)
         syntax/parse/define)

(provide
 case/dep)

(define-syntax-parser case/dep
  [(_ what-e:expr [dep-e:expr e ...] ...+)
   #'(let ([what what-e])
       (cond
         [(eq? what dep-e) e ...] ...))])
