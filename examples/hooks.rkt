#lang racket/gui/easy

(define/obs @choice 'a)

(render
 (window
  #:title "Lifecycle Hooks"
  (choice
   '(a b)
   #:choice->label symbol->string
   #:selection @choice
   (λ:= @choice))
  (dyn-view
   @choice
   (lambda (choice)
     (case choice
       [(a) (add-hooks
             #:on-create (λ () (eprintf "a created~n"))
             #:on-destroy (λ () (eprintf "a destroyed~n"))
             (text "a"))]
       [(b) (add-hooks
             #:on-create (λ () (eprintf "b created~n"))
             #:on-destroy (λ () (eprintf "b destroyed~n"))
             (text "b"))])))))
