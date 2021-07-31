#lang racket/base

(require racket/format
         racket/gui/easy
         racket/gui/easy/operator
         racket/list)

(define id-seq 0)
(define (next-id!)
  (begin0 id-seq
    (set! id-seq (add1 id-seq))))
(define @entries
  (@ (for/list ([_ (in-range 5)])
       (define id (next-id!))
       (cons id (~a id)))))

(define (shuffle!)
  (@entries . <~ . shuffle))

(define (exclaim!)
  (@entries . <~ . (λ (entries)
                     (for/list ([e (in-list entries)])
                       (cons (car e) (~a (cdr e) "!"))))))

(define (add-one-more!)
  (@entries . <~ . (λ (entries)
                     (define id (next-id!))
                     (append entries (list (cons id (~a id)))))))

(define (delete-last!)
  (@entries . <~ . (λ (entries)
                     (define len (length entries))
                     (if (zero? len) null (take entries (sub1 len))))))

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
     (button "Add one more!" add-one-more!)
     (button "Delete last!" delete-last!))
    (list-view
     @entries
     #:key car
     (λ (@entry)
       (input (@entry . ~> . cdr)
              (λ (event text)
                (case event
                  [(return)
                   (@entries . <~ . (λ (entries)
                                      (define k (car (obs-peek @entry)))
                                      (for/list ([e (in-list entries)])
                                        (if (eq? (car e) k)
                                            (cons k text)
                                            e))))]))))))))

(module+ main
  (render app))
