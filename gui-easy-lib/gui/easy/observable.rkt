#lang racket/base

(require racket/contract
         "private/observable.rkt")

(provide
 observer/c
 maybe-obs/c
 (contract-out
  [obs (-> any/c obs?)]
  [obs? (-> any/c boolean?)]
  [obs-observe! (-> obs? observer/c void?)]
  [obs-unobserve! (-> obs? observer/c void?)]
  [obs-update! (-> obs? (-> any/c any/c) any/c)]
  [obs-peek (-> obs? any/c)]
  [obs-map (-> obs? (-> any/c any/c) obs?)]
  [obs-combine (-> procedure? obs? obs? ... obs?)]))

(define observer/c
  (-> any/c any))

(define (maybe-obs/c c)
  (or/c c obs?))
