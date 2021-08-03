#lang racket/base

(require (for-syntax racket/base
                     racket/format
                     syntax/parse)
         "../logger.rkt"
         "../observable.rkt")

(provide
 case/dep
 peek)

(begin-for-syntax
  (define (trim-path p [max-length 30])
    (define s (path->string p))
    (cond
      [(< (string-length s) max-length) s]
      [else (~a "..." (substring s (- (string-length s) (- max-length 3))))])))

(define-syntax (case/dep stx)
  (syntax-parse stx
    [(_ what-e:expr [dep-e:expr e ...] ...+)
     (define-values (source line col pos)
       (values (syntax-source stx)
               (syntax-line stx)
               (syntax-column stx)
               (syntax-position stx)))
     (define loc
       (if (path-string? source)
           (if (and line col)
               (format "~a:~a:~a" (trim-path source) line col)
               (format "~a:~a" (trim-path source) pos))
           "unknown"))
     (with-syntax ([loc (datum->syntax stx loc)])
       #'(let ([what what-e])
           (cond
             [(let ([dep dep-e])
                (and (obs? dep)
                     (equal? what dep)))
              (log-gui-easy-debug "case/dep matched ~.s at ~a" 'dep-e loc)
              e ...]
             ...)))]))

(define (peek @v)
  (if (obs? @v) (obs-peek @v) @v))
