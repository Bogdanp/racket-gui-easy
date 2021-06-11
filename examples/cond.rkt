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
   #:label (@toggled? . ~> . (λ (toggled?)
                               (if toggled?
                                   "Toggled"
                                   "Not Toggled")))
   (checkbox
    #:label "Toggle"
    #:checked? @toggled?
    (@toggled? . λ<~ . not))
   (cond/view
    [@toggled?
     (cond/view
      [@password-correct?
       (vpanel
        (label "You're in!")
        (button "Log out"
                (λ ()
                  (@password . := . ""))))]
      [else
       (input @password
              (λ (_ text)
                (@password . := . text)))])]
    [else
     (label "Not Toggled")]))))
