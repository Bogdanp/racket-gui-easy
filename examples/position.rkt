#lang racket/base

(require racket/gui/easy
         racket/gui/easy/operator)

(define (pos x y) (cons x y))
(define (pos+x p amt) (cons (max 0 (+ amt (car p))) (cdr p)))
(define (pos+y p amt) (cons (car p) (max 0 (+ amt (cdr p)))))

(define @p (obs (pos 0 0)))
(render
 (window
  #:label (@p . ~> . (λ (p)
                       (format "Pos (~s, ~s)" (car p) (cdr p))))
  #:size (cons 100 100)
  #:position @p
  (vpanel
   (hpanel
    (button "Sub x" (@p . λ<~ . (λ (p) (pos+x p -1))))
    (button "Add x" (@p . λ<~ . (λ (p) (pos+x p  1)))))
   (hpanel
    (button "Sub y" (@p . λ<~ . (λ (p) (pos+y p -1))))
    (button "Add y" (@p . λ<~ . (λ (p) (pos+y p  1))))))))
