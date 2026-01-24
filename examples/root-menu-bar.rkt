#lang racket/gui/easy

(require racket/class)

(define t
  (new gui:timer%
       [notify-callback void]
       [interval 1000]))

(render-menu-bar
 (menu-bar
  (menu
   "File"
   (menu-item
    "Say Hi"
    (lambda ()
      (gui:message-box "Hello" "Hi")))
   (menu-item
    "Quit"
    (lambda ()
      (send t stop))))))
