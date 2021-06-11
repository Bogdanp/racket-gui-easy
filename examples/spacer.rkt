#lang racket/base

(require racket/gui/easy)

(render
 (window
  #:size (cons 400 400)
  (vpanel
   (hpanel
    (label "A")
    (spacer)
    (label "B"))
   (spacer)
   (hpanel
    (label "C")
    (spacer)
    (label "D")))))
