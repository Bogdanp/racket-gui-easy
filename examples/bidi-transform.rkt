#lang racket/base

(require racket/gui/easy
         racket/gui/easy/operator
         racket/string)

(define/obs @n 42)

(define (text->num text)
  (and (not (string-suffix? text "."))
       (string->number text)))

(render
 (window
  #:size '(200 100)
  (vpanel
   (input
    @n
    #:value=? =
    #:value->text number->string
    (λ (_event text)
      (define n (text->num text))
      (when n (@n . := . n))))
   (text (@n . ~> . number->string)))))
