#lang racket/base

(require racket/format
         racket/gui/easy
         racket/gui/easy/operator)

(struct character (id) #:transparent)
(struct human character (name hp) #:transparent)
(struct dog character (name hp) #:transparent)
(struct gremlin character (hp) #:transparent)
(struct state (characters editing-id) #:transparent)

(define (character-type c)
  (cond
    [(human? c) 'human]
    [(dog? c) 'dog]
    [(gremlin? c) 'gremlin]
    [else (raise-argument-error 'character-type "(or/c human? dog? gremlin?)" c)]))

(define (~character c)
  (case (character-type c)
    [(human) (human-name c)]
    [(dog) (dog-name c)]
    [(gremlin) (~a "Gremlin " (character-id c))]))

(define (title label)
  (hpanel
   #:stretch '(#t #f)
   #:alignment '(center top)
   (text label)))

(define (labeled label v)
  (hpanel
   #:stretch '(#t #f)
   (text label)
   v))

(define (human-editor @h)
  (vpanel
   (title "Human")
   (labeled "Name:" (input (@h . ~> . human-name)))
   (labeled "HP:" (input (@h . ~> . (compose1 ~a human-hp))))))

(define (dog-editor @d)
  (vpanel
   (title "Dog")
   (labeled "Name:" (input (@d . ~> . dog-name)))
   (labeled "HP:" (input (@d . ~> . (compose1 ~a dog-hp))))))

(define (gremlin-editor @g)
  (vpanel
   (title "Gremlin")
   (labeled "HP:" (input (@g . ~> . (compose1 ~a gremlin-hp))))))

(define/obs @state
  (state
   (list
    (human 0 "Bogdan" 100)
    (dog 1 "Doug" 30)
    (gremlin 2 50)
    (gremlin 3 50))
   0))

(define/obs @current-character
  (@state . ~> . (λ (s)
                   (findf
                    (λ (c) (= (character-id c) (state-editing-id s)))
                    (state-characters s)))))

;; While the `case-view' short circuits, changing the underlying type
;; of an observable isn't a completely safe operation because derived
;; observables aren't immediately garbage-collected when a view is
;; destroyed.
;;
;; For example, when `@current-character' is a `human', the
;; `human-editor' procedure creates a derived obervable that maps
;; `@current-character' to a string name using `human-name'.  After
;; that view is replaced, that mapping lingers for some time until it
;; is garbage-collected, and if the "type" of character changes, it
;; raises an error asynchronously.  That error can be safely ignored
;; since it's on a different thread, but that's not exactly pretty.
;; Instead, we use this procedure to ensure disjoint views always get
;; observables of the correct type by plugging in dummy values when
;; the type changes.
(define (coerce @char type)
  (@char . ~> . (λ (char)
                  (or
                   (and (eq? (character-type char) type) char)
                   (case type
                     [(human) (human -1 "Anon" 100)]
                     [(dog) (dog -1 "Anon" 30)]
                     [(gremlin) (gremlin -1 50)])))))

(render
 (window
  #:title "Character Editor"
  #:size '(800 600)
  (hpanel
   (table
    '("Character")
    (@state . ~> . (compose1 list->vector state-characters))
    #:entry->row (λ (char)
                   (vector (~character char)))
    (λ (event entries selection)
      (case event
        [(select)
         (when selection
           (@state . <~ . (λ (s)
                            (struct-copy state s [editing-id (character-id (vector-ref entries selection))]))))])))
   (case-view (@current-character . ~> . character-type)
     [(human) (human-editor (coerce @current-character 'human))]
     [(dog) (dog-editor (coerce @current-character 'dog))]
     [(gremlin) (gremlin-editor (coerce @current-character 'gremlin))]
     [else (text "Unreachable")]))))
