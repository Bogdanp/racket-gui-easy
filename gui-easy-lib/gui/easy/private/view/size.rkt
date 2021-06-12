#lang racket/base

(provide
 (struct-out size)
 (struct-out stretch))

(struct size (w h) #:transparent)
(struct stretch (w? h?) #:transparent)
