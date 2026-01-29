#lang racket/base

(require (for-syntax racket/base)
         racket/class
         version-case
         "../class.rkt")

(provide
 container%
 in-child-widgets
 with-container-sequence)

(version-case
 [(version>= (version) "8.12.0.7")
  (require racket/treelist)
  (define (make-child-widgets)
    (treelist))
  (define (append-child-widget ws w)
    (treelist-add ws w))
  (define-syntax in-child-widgets
    (make-rename-transformer #'in-treelist))]
 [else
  (define (make-child-widgets) ;; noqa
    null)
  (define (append-child-widget ws w) ;; noqa
    (append ws (list w)))
  (define-syntax in-child-widgets
    (make-rename-transformer #'in-list))])

;; A container needs to keep track of its children and what dependencies
;; map to which of those children. It also needs to keep track of the
;; widgets created for each of those children, for each of the container
;; widgets that were produced by a subclass of container% (eg. a window,
;; a panel, etc.). It does the former by keeping track of the child
;; view%s directly and generating a mapping (deps-to-children) from
;; dependencies to child view%s. It does the latter by storing a mapping
;; of child view%s to a list of GUI widgets on the context of the
;; container widget (get-children-to-widgets).
(define container%
  (class object%
    (init-private-field children)
    (super-new)

    (define deps-to-children (make-hash))
    (for* ([c (in-list children)]
           [d (in-list (send c dependencies))])
      (hash-update! deps-to-children d (λ (cs) (cons c cs)) null))

    (define/public (get-children)
      children)

    (define/public (child-dependencies)
      (hash-keys deps-to-children))

    (define/public (update-children v what val)
      (for* ([c (in-list (hash-ref deps-to-children what null))]
             [child-v (in-child-widgets (get-child-widgets v c))])
        (send c update child-v what val)))

    (define/public (destroy-children v)
      (define children-to-widgets (get-children-to-widgets v))
      (for* ([(c ws) (in-hash children-to-widgets)]
             [w (in-child-widgets ws)])
        (send c destroy w))
      (hash-clear! children-to-widgets))

    (define/public (add-child-widget v c w)
      (hash-update!
       #;ht (get-children-to-widgets v)
       #;key c
       #;updater
       (lambda (ws)
         (append-child-widget ws w))
       #;failure-result
       make-child-widgets))

    (define/public (get-child-widgets v c [failure-result make-child-widgets])
      (hash-ref (get-children-to-widgets v) c failure-result))

    (define/public (has-child-widgets? v c)
      (hash-has-key? (get-children-to-widgets v) c))

    (define/public (remove-child-widgets v c)
      (hash-remove! (get-children-to-widgets v) c))

    (define/private (get-children-to-widgets v)
      (send v get-context! 'children-to-widgets make-hasheq))))

(define-syntax-rule (with-container-sequence container body0 body ...)
  (dynamic-wind
    (lambda ()
      (send container begin-container-sequence))
    (lambda ()
      body0
      body ...)
    (lambda ()
      (send container end-container-sequence))))
