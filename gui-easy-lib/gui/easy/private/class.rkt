#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse/pre)
         racket/class)

(provide
 init-private-field)

(begin-for-syntax
  (define-syntax-class private-field-decl
    (pattern name:id
             #:attr init-name (generate-temporary #'name)
             #:attr decl #'[(init-name name)])
    (pattern [name:id default-expr:expr]
             #:attr init-name (generate-temporary #'name)
             #:attr decl #'[(init-name name) default-expr])
    (pattern [(init-name:id name:id)]
             #:attr decl #'[(init-name name)])
    (pattern [(init-name:id name:id) default-expr:expr]
             #:attr decl #'[(init-name name) default-expr])))

(define-syntax (init-private-field stx)
  (syntax-parse stx
    [(_ fld:private-field-decl ...)
     #'(begin
         (init fld.decl ...)
         (define fld.name fld.init-name) ...)]))

(module+ test
  (require racket/class
           rackunit)
  (define a-class%
    (class object%
      (init-private-field a b c)
      (super-new)
      (define/public (to-string)
        (format "~a ~a ~a" a b c))))
  (define an-ob
    (new a-class% [a 1] [b "b"] [c #t]))
  (check-equal? (send an-ob to-string) "1 b #t")
  (check-exn
   #rx"given object does not have the requested field"
   (lambda ()
     (get-field a an-ob))))
