#lang racket/base

(require racket/gui/easy
         racket/gui/easy/operator)

(define/obs @count 0)
(define/obs @view
  (@count . ~> . (λ (c)
                   (text (format "Count: ~a" c)))))

(thread
 (lambda ()
   (let loop ()
     (@count . <~ . add1)
     (sleep 1)
     (loop))))

(render
 (window
  #:size '(800 600)
  (observable-view @view)))
