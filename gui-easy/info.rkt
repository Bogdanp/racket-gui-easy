#lang info

(define collection "racket")
(define deps '("base"
               "gui-easy-lib"))
(define build-deps '("draw-doc"
                     "gui-doc"
                     "gui-lib"
                     "gui-easy-lib"
                     "pict-doc"
                     "pict-lib"
                     "racket-doc"
                     "scribble-lib"))
(define implies '("gui-easy-lib"))
(define update-implies '("gui-easy-lib"))
