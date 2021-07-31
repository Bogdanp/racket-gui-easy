#lang racket/base

(require racket/format
         racket/gui/easy
         racket/gui/easy/operator
         racket/list)

(define @entries
  (@ (for/list ([i (in-range 20)])
       (cons i (number->string i)))))

(define (shuffle!)
  (@entries . <~ . shuffle))

(define (exclaim!)
  (@entries . <~ . (λ (entries)
                     (for/list ([e (in-list entries)])
                       (cons (car e) (~a (cdr e) "!"))))))

(define app
  (window
   #:size '(800 600)
   #:stretch '(#t #t)
   (vpanel
    #:alignment '(left top)
    (hpanel
     #:stretch '(#t #f)
     (button "Shuffle!" shuffle!)
     (button "Exclaim!" exclaim!))
    (list-view
     @entries
     #:key car
     (λ (entry)
       (input (cdr entry)))))))

(module+ main
  (render app))
