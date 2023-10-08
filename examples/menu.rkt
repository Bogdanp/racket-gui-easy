#lang racket/gui/easy


(define/obs @can-save? #t)

(render
 (window
  #:size '(800 600)
  (menu-bar
   (menu "&File"
         (menu-item "&New File")
         (menu-item "&Open..." (λ () (gui:get-file)))
         (menu-item
          "&Save..."
          #:enabled? @can-save?
          #:help "Saves the file"
          #:shortcut (if (eq? (system-type 'os) 'macosx)
                         '(cmd #\s)
                         '(ctl #\s)))
         (menu-item-separator)
         (menu-item "&Print...")))
  (button
   "Toggle Save"
   (lambda ()
     (@can-save? . <~ . not)))))
