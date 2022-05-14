#lang racket/base

(require racket/gui/easy
         racket/gui/easy/operator)

(define @choices (@ '("A" "B" "C" "D")))
(define @selection (@ "A"))

(render
 (window
  #:size '(400 400)
  (tabs
   #:style '(no-border can-close can-reorder)
   @choices
   #:selection @selection
   (λ (event choices selection)
     (case event
       [(close)
        (@choices . := . choices)
        (@selection . := . selection)]
       [(reorder)
        (@choices . := . choices)]
       [(select)
        (@selection . := . selection)]))
   (case-view @selection
     [("A")
      (text "View A")]
     [("B")
      (text "View B")]
     [("C")
      (text "View C")]
     [("D")
      (text "View D")]
     [else
      (hpanel)]))))
