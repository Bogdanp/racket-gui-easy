#lang racket/base

(require racket/class
         racket/contract
         (prefix-in gui: racket/gui)
         "private/renderer.rkt"
         "private/view.rkt")

(provide
 (contract-out
  [render (->* ((is-a?/c window-view<%>))
               ((or/c (is-a?/c gui:frame%)
                      (is-a?/c gui:dialog%)
                      #f))
               (is-a?/c renderer<%>))]
  [renderer-root (-> (is-a?/c renderer<%>)
                     (or/c (is-a?/c gui:frame%) #f))]))
