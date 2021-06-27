#lang racket/base

(require (prefix-in gui: racket/gui)
         racket/gui/easy)

(render
 (window
  #:size '(800 600)
  (menu-bar
   (menu "&File"
         (menu-item "&New File")
         (menu-item "&Open File..." (λ () (gui:get-file)))
         (menu-item-separator)
         (menu-item "&Print...")))))
