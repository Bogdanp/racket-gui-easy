#lang racket/base

(require racket/gui/easy
         racket/gui/easy/operator)

(define @toggled? (@ #f))
(define @password (@ ""))
(define @password-correct?
  (@password . ~> . (λ (p)
                      (string=? p "opensesame"))))

(void
 (render
  (window
   #:title (@toggled? . ~> . (λ (toggled?)
                               (if toggled?
                                   "Toggled"
                                   "Not Toggled")))
   (checkbox
    #:label "Toggle"
    #:checked? @toggled?
    (λ:= @toggled?))
   (cond-view
    [@toggled?
     (cond-view
      [@password-correct?
       (vpanel
        (text "You're in!")
        (button "Log out"
                (λ ()
                  (@password . := . ""))))]
      [else
       (input @password
              (λ (_ text)
                (@password . := . text)))])]
    [else
     (text "Not Toggled")]))))
