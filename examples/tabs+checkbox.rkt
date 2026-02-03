#lang racket/gui/easy

(define/obs @tab "A")
(define/obs @checked? #f)

(define cb
  (checkbox
   (λ:= @checked?)
   #:label "Check Me"
   #:checked? @checked?))

(render
 (window
  (tabs
   '("A" "B")
   #:selection @tab
   (lambda (_event _tabs selection)
     (@tab . := . selection))
   (observable-view
    @tab
    (lambda (tab)
      (vpanel
       (text tab)
       cb))))))
