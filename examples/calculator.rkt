#lang racket/base

(require racket/format
         racket/gui/easy
         racket/gui/easy/operator
         racket/match)

(define @nums (@ '(waiting (0))))
(define @op (@ #f))
(define @out
  (@nums . ~> . (λ (nums)
                  (match nums
                    [`(,_ ,(cons n _)) (number->string n)]))))

(define (disp @o)
  (input @o #:enabled? #f))

(define (num n)
  (button
   (~a n)
   (@nums . λ<~ . (match-lambda
                    [`(ready ,st)
                     `(waiting ,(cons n st))]
                    [`(waiting ,st)
                     `(waiting ,(cons (+ (* (car st) 10) n) (cdr st)))]))))

(define (op f)
  (button
   (~a (object-name f))
   (λ ()
     (match (obs-peek @nums)
       [`(,_ (,b ,a))
        (@op . <~ . (λ (current-op)
                      (begin0 f
                        (@nums . := . `(ready (,(current-op a b)))))))]

       [`(,_ ,st)
        (@nums . := . `(ready ,st))
        (@op   . := . f)]))))

(define (cls)
  (button "AC" (λ ()
                 (@nums . := . '(waiting (0)))
                 (@op   . := . #f))))

(define (eq)
  (button "=" (λ ()
                (match* ((obs-peek @op) (obs-peek @nums))
                  [(op `(,_ (,b ,a))) #:when op
                   (@nums . := . `(ready (,(op a b))))
                   (@op   . := . #f)]

                  [(_ _) (void)]))))

(render
 (window
  #:title @out
  (hpanel (disp @out) (cls) (eq))
  (hpanel (num 7)  (num 8) (num 9)  (op +))
  (hpanel (num 4)  (num 5) (num 6)  (op -))
  (hpanel (num 1)  (num 2) (num 3)  (op *))
  (hpanel (spacer) (num 0) (spacer) (op /))))
