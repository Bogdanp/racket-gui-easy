#lang racket/gui/easy

(define/obs @menu-bar-enabled? #t)
(define/obs @file-menu-enabled? #t)
(define/obs @can-save? #t)
(define/obs @checked? #f)

(render
 (window
  #:size '(800 600)
  (menu-bar
   #:enabled? @menu-bar-enabled?
   (menu
    "&File"
    #:enabled? @file-menu-enabled?
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
    (checkable-menu-item
     (@checked? . ~> . (λ (checked?) (if checked?
                                         "This is checked"
                                         "This is not checked")))
     (λ:= @checked?)
     #:checked? @checked?)))
  (vpanel
   (button
    "Toggle Menu Bar"
    (lambda ()
      (@menu-bar-enabled? . <~ . not)))
   (button
    "Toggle File Menu"
    (lambda ()
      (@file-menu-enabled? . <~ . not)))
   (button
    "Toggle Save"
    (lambda ()
      (@can-save? . <~ . not))))))
