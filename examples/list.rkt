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

(define (add-one-more!)
  (@entries . <~ . (λ (entries)
                     (define len (length entries))
                     (append entries (list (cons len (~a len)))))))

(define app
  (window
   #:size '(800 600)
   #:stretch '(#t #t)
   (vpanel
    #:alignment '(left top)
    (hpanel
     #:stretch '(#t #f)
     (button "Shuffle!" shuffle!)
     (button "Exclaim!" exclaim!)
     (button "Add one more!" add-one-more!))
    (list-view
     @entries
     #:key car
     (λ (entry)
       (input (cdr entry)
              (λ (event text)
                (case event
                  [(return)
                   (@entries . <~ . (λ (entries)
                                      (for/list ([e (in-list entries)])
                                        (define k (car e))
                                        (if (equal? k (car entry))
                                            (cons (car e) text)
                                            e))))]))))))))

(module+ main
  (render app))
