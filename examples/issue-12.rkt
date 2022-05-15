#lang racket/base

(require racket/gui/easy
         racket/gui/easy/operator)

(define/obs @text "")
(define/obs @num #f)

(render
 (window
  (vpanel
   (hpanel
    (vpanel
     (text "Free-form")
     (input @text))
    (vpanel
     (text "Numeric")
     (input @num
            #:value->text
            (λ (num) (if num (number->string num) ""))
            (λ (event text)
              (case event
                [(return)
                 (@num . := . (string->number text))])))))
   (button "Clear" (λ ()
                     (@text . := . "")
                     (@num  . := . #f))))))
