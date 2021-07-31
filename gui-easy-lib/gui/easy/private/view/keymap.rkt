#lang racket/base

(require (only-in framework keymap:get-global)
         racket/class)

(provide
 the-default-keymap)

(define the-default-keymap
  (keymap:get-global))

(define (add&map keyname name proc [k the-default-keymap])
  (define name-str (symbol->string name))
  (send k add-function name-str proc)
  (send k map-function keyname name-str))

(when (eq? 'macosx (system-type 'os))
  (add&map "d:a" 'select-all (λ (editor _event)
                               (send editor do-edit-operation 'select-all))))
