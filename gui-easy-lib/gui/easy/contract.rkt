#lang racket/base

(require racket/contract
         (prefix-in gui: racket/gui))

(provide (all-defined-out))

(define alignment/c
  (list/c (or/c 'left 'center 'right)
          (or/c 'top 'center 'bottom)))

(define margin/c
  (list/c gui:spacing-integer?
          gui:spacing-integer?))

(define maybe-label/c
  (or/c #f gui:label-string?))

(define position/c
  (or/c 'center (list/c gui:position-integer?
                        gui:position-integer?)))

(define size/c
  (list/c (or/c #f gui:dimension-integer?)
          (or/c #f gui:dimension-integer?)))

(define spacing/c
  gui:spacing-integer?)

(define stretch/c
  (list/c boolean? boolean?))
