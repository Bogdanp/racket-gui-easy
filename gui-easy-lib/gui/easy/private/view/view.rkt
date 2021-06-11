#lang racket/base

(require racket/class)

(provide
 view<%>)

(define view<%>
  (interface () dependencies create update destroy))
