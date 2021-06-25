#lang racket/base

(require racket/gui/easy
         racket/gui/easy/operator
         racket/string)

(define @choice-input (@ "a,b,c"))
(define @choices (@choice-input . ~> . (λ (s)
                                         (map string-trim (string-split s "," #:trim? #f)))))
(define @choice (@ "a"))

(render
 (window
  (input @choice-input (λ (_ text)
                         (@choice-input . := . text)))
  (text @choice)
  (choice
   @choices
   #:selection @choice
   (λ (c)
     (@choice . := . (if c c "<no choices>"))))))
