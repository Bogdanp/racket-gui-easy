#lang racket/base

(require racket/gui/easy
         racket/gui/easy/operator)

(define (pos x y) (list x y))
(define (pos+x p amt) (list (max 0 (+ amt (car p))) (cadr p)))
(define (pos+y p amt) (list (car p) (max 0 (+ amt (cadr p)))))

(define @p (obs (pos 0 0)))
(render
 (window
  #:title (@p . ~> . (λ (p)
                       (format "Pos (~s, ~s)" (car p) (cadr p))))
  #:size '(100 100)
  #:position @p
  (vpanel
   (hpanel
    (button "Sub x" (@p . λ<~ . (λ (p) (pos+x p -1))))
    (button "Add x" (@p . λ<~ . (λ (p) (pos+x p  1)))))
   (hpanel
    (button "Sub y" (@p . λ<~ . (λ (p) (pos+y p -1))))
    (button "Add y" (@p . λ<~ . (λ (p) (pos+y p  1))))))))
