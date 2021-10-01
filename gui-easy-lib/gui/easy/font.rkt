#lang racket/base

(require racket/class
         racket/contract
         (prefix-in gui: racket/gui))

(provide
 (contract-out
  [font (->* (string? (real-in 0 1024.0))
             (#:family gui:font-family/c
              #:style gui:font-style/c
              #:weight gui:font-weight/c
              #:underline? any/c
              #:smoothing gui:font-smoothing/c
              #:hinting gui:font-hinting/c
              #:size-in-pixels? any/c)
             (is-a?/c gui:font%))]))

(define (font face size
              #:family [family 'default]
              #:style [style 'normal]
              #:weight [weight 'normal]
              #:underline? [underline? #f]
              #:smoothing [smoothing 'default]
              #:hinting [hinting 'aligned]
              #:size-in-pixels? [pixels? #f])
  (send gui:the-font-list find-or-create-font size face family style weight underline? smoothing pixels? hinting))
