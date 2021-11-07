#lang racket/base

(require racket/class
         (prefix-in gui: racket/gui)
         racket/gui/easy
         racket/gui/easy/operator)

(define f
  (new gui:frame%
       [label "Counter"]))

(define p
  (new gui:vertical-panel%
       [parent f]))

(define (counter start)
  (define/obs @count start)
  (hpanel
   (button "-" (@count . λ<~ . sub1))
   (text (@count . ~> . number->string))
   (button "+" (@count . λ<~ . add1))))

(define renderers
  (for/list ([i (in-range 3)])
    (embed p (counter i))))

(send f show #t)

(module+ test
  (require racket/gui/easy/debugger)
  (start-debugger))
