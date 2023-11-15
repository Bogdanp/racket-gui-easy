#lang racket/base

(require racket/contract
         "private/observable.rkt")

(provide
 (contract-out
  [obs (->* (any/c)
            (#:name symbol?
             #:derived? boolean?)
            obs?)]
  [obs? (-> any/c boolean?)]
  [obs-rename (-> obs? symbol? obs?)]
  [obs-observe! (-> obs? (-> any/c any) void?)]
  [obs-unobserve! (-> obs? (-> any/c any) void?)]
  [obs-update! (-> obs? (-> any/c any/c) any/c)]
  [obs-set! (-> obs? any/c void?)]
  [obs-peek (-> obs? any/c)]
  [obs-map (-> obs? (-> any/c any/c) obs?)]
  [obs-filter (->* (obs? (-> any/c any/c)) (any/c) obs?)]
  [obs-filter-map (->* (obs? (-> any/c any/c)) (any/c) obs?)]
  [obs-combine (-> procedure? obs? obs? ... obs?)]
  [obs-debounce (->* (obs?)
                     (#:duration exact-nonnegative-integer?)
                     obs?)]
  [obs-throttle (->* (obs?)
                     (#:duration exact-nonnegative-integer?)
                     obs?)]))
