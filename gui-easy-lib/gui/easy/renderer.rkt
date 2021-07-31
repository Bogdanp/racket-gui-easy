#lang racket/base

(require racket/class
         racket/contract
         (prefix-in gui: racket/gui)
         "private/renderer.rkt"
         "private/view.rkt")

(provide
 renderer?
 (contract-out
  [render (->* ((is-a?/c window-view<%>))
               ((or/c (is-a?/c renderer<%>) #f))
               (is-a?/c renderer<%>))]
  [render-popup-menu (-> (is-a?/c renderer<%>)
                         (is-a?/c popup-menu-view<%>)
                         gui:position-integer?
                         gui:position-integer?
                         void?)]))

(define (renderer? v)
  (is-a? v renderer<%>))
