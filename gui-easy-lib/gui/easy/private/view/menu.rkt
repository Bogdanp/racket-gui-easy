#lang racket/base

(require racket/class
         racket/contract/base
         (prefix-in gui: racket/gui)
         racket/list
         "../class.rkt"
         "../observable.rkt"
         "common.rkt"
         "container.rkt"
         "view.rkt")

(provide
 popup-menu-view<%>
 popup-menu
 menu-bar-view<%>
 menu-bar
 menu-view<%>
 menu
 menu-item
 checkable-menu-item
 menu-item-separator)

(define popup-menu-view<%>
  (interface (view<%>)
    [create (->m #f (is-a?/c gui:popup-menu%))]))

(define (make-popup-menu% gui-popup-menu%)
  (class* container% (popup-menu-view<%>)
    (inherit add-child get-children update-children destroy-children child-dependencies)
    (super-new)

    (define/public (dependencies)
      (child-dependencies))

    (define/public (create _parent)
      (define the-menu (new (context-mixin gui-popup-menu%)))
      (begin0 the-menu
        (for ([c (in-list (get-children))])
          (add-child the-menu c (send c create the-menu)))))

    (define/public (update v what val)
      (update-children v what val))

    (define/public (destroy v)
      (destroy-children v))))

(define menu-bar-view<%>
  (interface (view<%>)
    [create (->m (or/c (is-a?/c gui:area<%>)
                       'root)
                 (or/c (is-a?/c gui:menu-bar%)
                       #f))]))

(define (make-menu-bar% gui-menu-bar%)
  ;; would be nicer if `racket/gui` could report an existing root menu bar
  (define get-root-menu-bar
    (let ([root-menu-bar #f])
      (lambda ()
        (unless root-menu-bar
          (define mb
            (new (context-mixin gui-menu-bar%)
                 [parent 'root]))
          (set! root-menu-bar mb))
        root-menu-bar)))
  (class* container% (menu-bar-view<%>)
    (init-private-field @enabled?)
    (inherit add-child get-children update-children destroy-children child-dependencies)
    (super-new)

    (define/public (dependencies)
      (remove-duplicates
       (append (filter obs? (list @enabled?))
               (child-dependencies))))

    (define/public (create parent)
      (cond
        [(and (eq? parent 'root)
              (not (gui:current-eventspace-has-menu-root?)))
         #f]
        [else
         (define the-menu-bar
           (cond
             [(eq? parent 'root)
              (get-root-menu-bar)]
             [else
              (define top-level
                (send parent get-top-level-window))
              (or
               (send top-level get-menu-bar)
               (new (context-mixin gui-menu-bar%)
                    [parent top-level]))]))
         (begin0 the-menu-bar
           (send the-menu-bar enable (peek @enabled?))
           (for ([c (in-list (get-children))])
             (add-child the-menu-bar c (send c create the-menu-bar))))]))

    (define/public (update v what val)
      (case/dep what
        [@enabled? (send v enable val)])
      (update-children v what val))

    (define/public (destroy v)
      (destroy-children v))))

(define menu-view<%>
  (interface (view<%>)
    [create (->m (or/c (is-a?/c gui:menu-bar%)
                       (is-a?/c gui:popup-menu%)
                       (is-a?/c gui:menu%))
                 (is-a?/c gui:menu%))]))

(define (make-menu% gui-menu%)
  (class* container% (menu-view<%>)
    (init-private-field @label @enabled? @help)
    (inherit add-child get-children update-children destroy-children child-dependencies)
    (super-new)

    (define/public (dependencies)
      (remove-duplicates
       (append (filter obs? (list @label @enabled? @help))
               (child-dependencies))))

    (define/public (create parent)
      (define the-menu
        (new (context-mixin gui-menu%)
             [parent parent]
             [help-string (peek @help)]
             [label (peek @label)]))
      (begin0 the-menu
        (send the-menu enable (peek @enabled?))
        (for ([c (in-list (get-children))])
          (add-child the-menu c (send c create the-menu)))))

    (define/public (update v what val)
      (case/dep what
        [@enabled? (send v enable val)]
        [@help (send v set-help-string val)]
        [@label (send v set-label val)])
      (update-children v what val))

    (define/public (destroy v)
      (destroy-children v)
      (send v delete))))

(define (make-menu-item% gui-menu-item%)
  (class* object% (view<%>)
    (init-private-field @label @enabled? @help @shortcut action)
    (super-new)

    (define/public (dependencies)
      (filter obs? (list @label @enabled? @help @shortcut)))

    (define/public (create parent)
      (define the-item
        (new gui-menu-item%
             [parent parent]
             [help-string (peek @help)]
             [label (peek @label)]
             [callback (λ (_self _event)
                         (action))]))
      (begin0 the-item
        (send the-item enable (peek @enabled?))
        (set-shortcut the-item (peek @shortcut))))

    (define/public (update v what val)
      (case/dep what
        [@enabled? (send v enable val)]
        [@help (send v set-help-string val)]
        [@label (send v set-label val)]
        [@shortcut (set-shortcut v val)]))

    (define/public (destroy v)
      (send v delete))))

(define (make-checkable-menu-item% gui-checkable-menu-item%)
  (class* object% (view<%>)
    (init-private-field @label @checked? @enabled? @help @shortcut action)
    (super-new)

    (define/public (dependencies)
      (filter obs? (list @label @checked? @enabled? @help @shortcut)))

    (define/public (create parent)
      (define the-item
        (new gui-checkable-menu-item%
             [parent parent]
             [help-string (peek @help)]
             [label (peek @label)]
             [callback (λ (_self _event)
                         (action (send _self is-checked?)))]
             [checked (peek @checked?)]))
      (begin0 the-item
        (send the-item enable (peek @enabled?))
        (set-shortcut the-item (peek @shortcut))))

    (define/public (update v what val)
      (case/dep what
        [@checked? (send v check val)]
        [@enabled? (send v enable val)]
        [@help (send v set-help-string val)]
        [@label (send v set-label val)]
        [@shortcut (set-shortcut v val)]))

    (define/public (destroy v)
      (send v delete))))

(define (set-shortcut v s)
  ;; Contract guarantees at least one prefix and one key.
  (define-values (p k)
    (for/fold ([p null] [k #f] #:result (values (reverse p) k))
              ([v (in-list (or s null))])
      (values (if k (cons k p) p) v)))
  (send v set-shortcut k)
  (send v set-shortcut-prefix p))

(define (make-menu-item-separator% gui-separator-menu-item%)
  (class* object% (view<%>)
    (super-new)

    (define/public (dependencies)
      null)

    (define/public (create parent)
      (new gui-separator-menu-item%
           [parent parent]))

    (define/public (update _v _what _val)
      (void))

    (define/public (destroy v)
      (send v delete))))

(define (popup-menu #:mixin [mix values]
                    . children)
  (new (make-popup-menu% (mix gui:popup-menu%))
       [children children]))

(define (menu-bar #:enabled? [@enabled? (obs #t)]
                  #:mixin [mix values]
                  . children)
  (new (make-menu-bar% (mix gui:menu-bar%))
       [@enabled? @enabled?]
       [children children]))

(define (menu @label
              #:enabled? [@enabled? (obs #t)]
              #:help [@help (obs #f)]
              #:mixin [mix values]
              . children)
  (new (make-menu% (mix gui:menu%))
       [@label @label]
       [@enabled? @enabled?]
       [@help @help]
       [children children]))

(define (menu-item @label [action void]
                   #:enabled? [@enabled? (obs #t)]
                   #:help [@help (obs #f)]
                   #:shortcut [@shortcut (obs #f)]
                   #:mixin [mix values])
  (new (make-menu-item% (mix gui:menu-item%))
       [@label (->obs @label)]
       [@enabled? (->obs @enabled?)]
       [@help (->obs @help)]
       [@shortcut (->obs @shortcut)]
       [action action]))

(define (checkable-menu-item @label [action void]
                             #:checked? [@checked? (obs #f)]
                             #:enabled? [@enabled? (obs #t)]
                             #:help [@help (obs #f)]
                             #:shortcut [@shortcut (obs #f)]
                             #:mixin [mix values])
  (new (make-checkable-menu-item% (mix gui:checkable-menu-item%))
       [@label (->obs @label)]
       [@checked? (->obs @checked?)]
       [@enabled? (->obs @enabled?)]
       [@help (->obs @help)]
       [@shortcut (->obs @shortcut)]
       [action action]))

(define (menu-item-separator #:mixin [mix values])
  (new (make-menu-item-separator% (mix gui:separator-menu-item%))))
