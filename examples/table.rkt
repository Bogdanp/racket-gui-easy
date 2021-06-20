#lang racket/base

(require racket/gui/easy
         racket/gui/easy/operator
         racket/string)

(define @csv (@ "a,b"))
(define @entries (@csv . ~> . (λ (csv)
                                (for/vector ([row (in-list (string-split csv "\n"))])
                                  (for/vector ([col (in-list (string-split row ","))])
                                    col)))))
(define @selection (@ #f))

(define (number->string* n)
  (if n (number->string n) ""))

(render
 (window
  (input
   #:style '(multiple)
   @csv
   (λ (_ text)
     (@csv . := . text)))
  (choice
   #:label "Selection:"
   #:selection (@selection . ~> . number->string*)
   (@entries . ~> . (λ (entries)
                      (cons ""
                            (for/list ([i (in-naturals)]
                                       [_ (in-vector entries)])
                              (number->string i)))))
   (@selection . λ:= . string->number))
  (table
   '("A" "B")
   #:selection @selection
   @entries
   (λ (e _es v)
     (case e
       [(select)
        (@selection . := . v)])))))
