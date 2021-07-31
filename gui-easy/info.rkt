#lang info

(define collection "racket")
(define deps '("base"
               "gui-easy-lib"))
(define build-deps '("gui-doc"
                     "gui-lib"
                     "gui-easy-lib"
                     "racket-doc"
                     "scribble-lib"))
(define implies '("gui-easy-lib"))
(define update-implies '("gui-easy-lib"))
