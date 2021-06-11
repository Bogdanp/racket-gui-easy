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
    (inherit children-for-obs unique-obs
             get-child-frames add-child-frame get-child-frame remove-child-frame)
    (super-new)

    (define/public (dependencies)
      (unique-obs))

    (define/public (create parent)
      (define the-panel
        (new clazz
             [parent parent]))
      (begin0 the-panel
        (for ([c (in-list children)])
          (add-child-frame c (send c create the-panel)))))

    (define/public (update _v what val)
      (for ([c (in-list (children-for-obs what))])
        (send c update (get-child-frame c) what val)))

    (define/public (destroy v)
      (for ([(c v) (in-hash (get-child-frames))])
        (send c destroy v)
        (remove-child-frame c)))))

(define (hpanel . children)
  (new panel%
       [clazz gui:horizontal-panel%]
       [children children]))

(define (vpanel . children)
  (new panel%
       [clazz gui:vertical-panel%]
       [children children]))
