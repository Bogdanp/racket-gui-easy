#lang racket/base

(require racket/contract
         (prefix-in gui: racket/gui)
         "private/observable.rkt")

(provide (all-defined-out))

(define alignment/c
  (list/c (or/c 'left 'center 'right)
          (or/c 'top 'center 'bottom)))

(define margin/c
  (list/c gui:spacing-integer?
          gui:spacing-integer?))

(define maybe-label/c
  (or/c #f gui:label-string?))

(define position/c
  (or/c 'center (list/c gui:position-integer?
                        gui:position-integer?)))

(define size/c
  (list/c (or/c #f gui:dimension-integer?)
          (or/c #f gui:dimension-integer?)))

(define spacing/c
  gui:spacing-integer?)

(define stretch/c
  (list/c boolean? boolean?))

(define (obs/c c)
  (cond
    [(eq? any/c c) any/c]
    [else
     (make-contract
      #:name `(obs/c ,(contract-name c))
      #:late-neg-projection
      (λ (blame)
        (λ (o neg-party)
          (define make-check-value (contract-late-neg-projection c))
          (define check-init-value (make-check-value blame))
          (define check-updated-value (make-check-value (blame-swap blame)))
          (cond
            [(obs? o)
             (check-init-value (obs-peek o) neg-party)
             (impersonate-obs o
                              (λ (_ name) name)
                              (λ (_ update-proc)
                                (chaperone-procedure
                                 update-proc
                                 (λ (proc)
                                   (values
                                    (λ (v)
                                      (check-updated-value v neg-party)
                                      v)
                                    proc)))))]

            [else
             (raise-blame-error
              #:missing-party neg-party
              blame o
              '(expected: "~a" given: "~e") (contract-name c) o)]))))]))

(define (maybe-obs/c c)
  (cond
    [(eq? c any/c) any/c]
    [else (or/c c (obs/c c))]))

(module+ test
  (require rackunit)

  (test-case "(maybe-obs/c any/c)"
    (check-eq? (maybe-obs/c any/c) any/c))

  (test-case "(obs/c any/c)"
    (define/contract o (obs/c any/c) (obs 1))
    (check-equal? (obs-peek o) 1)
    (check-equal? (obs-update! o add1) 2))

  (test-case "(obs/c (>=/c 5))"
    (define/contract o (obs/c (>=/c 5)) (obs 5))
    (check-equal? (obs-peek o) 5)
    (check-equal? (obs-update! o values) 5)
    (check-exn
     #rx"expected: \\(>=/c 5\\)"
     (λ ()
       (obs-update! o sub1)))
    (check-exn
     #rx"promised: \\(>=/c 5\\)"
     (λ ()
       (contract (obs/c (>=/c 5)) (obs 3) 'pos 'neg #f #f)))))
