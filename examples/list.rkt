#lang racket/base

(require box-extra
         racket/format
         racket/gui/easy
         racket/gui/easy/operator
         racket/list)

(define id-box (box 0))
(define next-id! (let ([update! (make-box-update-proc id-box)])
                   (λ () (update! add1))))

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

(define (update-by-key! k text)
  (@entries . <~ . (λ (entries)
                     (for/list ([e (in-list entries)])
                       (if (eq? (car e) k)
                           (cons k text)
                           e)))))

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
     (λ (k @entry)
       (input (@entry . ~> . cdr)
              (λ (event text)
                (case event
                  [(return)
                   (update-by-key! k text)]))))))))

(module+ main
  (render app))
