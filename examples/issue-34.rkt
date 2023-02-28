#lang racket/base

(require racket/gui/easy
         racket/gui/easy/operator)

(define/obs @data
  '((1 #t)
    (2 #f)))

(void
 (render
  (window
   #:size '(800 600)
   (list-view
    @data
    #:key car
    (λ (_k @item)
      (hpanel
       (if-view (@item . ~> . cadr)
                (text "on")
                (text "off")))))
   (button
    "Flip"
    (λ ()
      (@data . <~ . (λ (ps)
                      (for/list ([p (in-list ps)])
                        (list (car p) (not (cadr p)))))))))))
