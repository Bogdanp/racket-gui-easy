#lang racket/base

(require racket/format
         racket/gui/easy
         racket/gui/easy/operator
         racket/list)

(define @choices (@ '(a b c)))
(define @selection (@ 'b))

(render
 (window
  (vpanel
   (choice
    @choices
    #:choice->label ~a
    #:choice=? eq?
    #:selection @selection
    (λ (selection)
      (@selection . := . selection)))
   (hpanel
    (button
     "Next"
     (@selection . λ<~ . (λ (sel)
                           (define cs (obs-peek @choices))
                           (define next-idx (modulo (add1 (index-of cs sel)) (length cs)))
                           (list-ref cs next-idx))))
    (button
     "Shuffle"
     (@choices . λ<~ . shuffle))))))
