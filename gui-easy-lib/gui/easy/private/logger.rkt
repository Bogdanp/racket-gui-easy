#lang racket/base

(provide (all-defined-out))
(define-logger gui-easy)
(define-logger gui-easy-data)

(define (log-change obs before after)
  (log-message gui-easy-data-logger 'debug #f "" (list obs before after) #t))

(define (make-change-evt)
  (handle-evt
   (make-log-receiver gui-easy-data-logger 'debug)
   (λ (v) (apply values (vector-ref v 2)))))
