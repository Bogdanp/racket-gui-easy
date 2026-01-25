#lang racket/base

(require racket/class
         racket/contract/base
         (prefix-in gui: racket/gui)
         "private/renderer.rkt"
         "private/view.rkt")

(provide
 renderer?
 (contract-out
  [embed (-> (is-a?/c gui:area<%>)
             (is-a?/c view<%>)
             (is-a?/c renderer<%>))]
  [render (->* [(is-a?/c window-view<%>)]
               [(or/c (is-a?/c renderer<%>) #f)
                #:wait? any/c]
               (or/c (is-a?/c renderer<%>) void?))]
  [render-popup-menu (->* [(is-a?/c renderer<%>)
                           (is-a?/c popup-menu-view<%>)
                           gui:position-integer?
                           gui:position-integer?]
                          [#:wait? any/c]
                          (or/c (is-a?/c renderer<%>) void?))]
  [render-menu-bar (-> (is-a?/c menu-bar-view<%>)
                       (is-a?/c renderer<%>))]
  [renderer-root (-> (is-a?/c renderer<%>) any/c)]
  [renderer-destroy (-> (is-a?/c renderer<%>) void?)]))

(define (renderer? v)
  (is-a? v renderer<%>))
