#lang racket/base

(require racket/gui/easy
         racket/gui/easy/operator)

(define @choices (@ '("A" "B" "B")))
(define @selection-index (@ 0))
(define @selection
  (obs-combine
   (λ (choices index)
     (list-ref choices index))
   @choices @selection-index))

(render
 (window
  #:size '(400 400)
  (tabs
   #:style '(no-border can-close can-reorder)
   @choices
   (λ (event choices index)
     (case event
       [(close)
        (when (= (obs-peek @selection-index) index)
          (@selection-index . := . (if (null? choices) #f 0)))
        (@choices . := . (for/list ([c (in-list choices)]
                                    [i (in-naturals)]
                                    #:unless (= i index))
                           c))]
       [(reorder)
        (@choices . := . choices)
        (@selection-index . := . index)]
       [(select)
        (@selection-index . := . index)]))
   (cond-view
    [(@selection . ~> . (λ (s) (equal? s "A")))
     (text "View A")]
    [(@selection . ~> . (λ (s) (equal? s "B")))
     (text "View B")]
    [else
     (hpanel)]))))
