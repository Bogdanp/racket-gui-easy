#lang info

(define license 'BSD-3-Clause)
(define collection "racket")
(define deps '("base"
               "gui-easy-lib"))
(define build-deps '("draw-doc"
                     "gui-doc"
                     "gui-lib"
                     "pict-doc"
                     "pict-lib"
                     "racket-doc"
                     "scribble-lib"))
(define implies '("gui-easy-lib"))
(define update-implies '("gui-easy-lib"))
