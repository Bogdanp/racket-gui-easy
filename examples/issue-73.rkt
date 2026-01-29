#lang racket/gui/easy

(define/obs @name "Harry")

(define greeter
  (vpanel
   (input @name (λ (_ str) (@name . := . str)))
   (text (@name . ~> . (λ (str) (string-append "Hello, " str "!"))))))

(render
 (window
  greeter
  greeter))
