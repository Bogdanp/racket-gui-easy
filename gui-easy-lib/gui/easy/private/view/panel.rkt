#lang racket/base

(require racket/class
         (prefix-in gui: racket/gui)
         "container.rkt"
         "view.rkt")

(provide
 hpanel
 vpanel)

(define panel%
  (class* container% (view<%>)
    (inherit-field children)
    (init-field clazz)
    (inherit children-for-dep child-dependencies
             get-children add-child get-child remove-child)
    (super-new)

    (define/public (dependencies)
      (child-dependencies))

    (define/public (create parent)
      (define the-panel
        (new clazz
             [parent parent]))
      (begin0 the-panel
        (for ([c (in-list children)])
          (add-child c (send c create the-panel)))))

    (define/public (update _v what val)
      (for ([c (in-list (children-for-dep what))])
        (send c update (get-child c) what val)))

    (define/public (destroy _v)
      (for ([(c w) (in-hash (get-children))])
        (send c destroy w)
        (remove-child c)))))

(define (hpanel . children)
  (new panel%
       [clazz gui:horizontal-panel%]
       [children children]))

(define (vpanel . children)
  (new panel%
       [clazz gui:vertical-panel%]
       [children children]))
