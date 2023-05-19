#lang racket/gui/easy

;; Inspired by https://tonsky.me/blog/humble-signals/

(struct todo (id text done?))
(struct todos (seq entries))

(define (change-todos ts proc)
  (define next-id (add1 (todos-seq ts)))
  (todos next-id (proc next-id (todos-entries ts))))

(define (append-todo ts text [done? #f])
  (change-todos ts (λ (id entries)
                     (append entries (list (todo id text done?))))))

(define (prepend-todo ts text [done? #f])
  (change-todos ts (λ (id entries)
                     (cons (todo id text done?) entries))))

(define (update-todo ts id proc)
  (change-todos ts (λ (_id entries)
                     (for/list ([t (in-list entries)])
                       (if (eqv? id (todo-id t))
                           (proc t)
                           t)))))

(define (remove-todo ts id)
  (change-todos ts (λ (_id entries)
                     (filter (λ (t) (not (eqv? id (todo-id t)))) entries))))

(define/obs @tab 'all)
(define/obs @todos
  (append-todo (todos 0 null) "First Todo"))
(define/obs @entries
  (obs-combine
   (lambda (tab ts)
     (define entries
       (todos-entries ts))
     (case tab
       [(all) entries]
       [(active) (filter (λ (t) (not (todo-done? t))) entries)]
       [(done) (filter (λ (t) (todo-done? t)) entries)]))
   @tab @todos))

(define (todo-view @t action)
  (hpanel
   #:stretch '(#t #f)
   (checkbox
    #:checked? (@t . ~> . todo-done?)
    (λ (done?)
      (action 'check done?)))
   (input
    (@t . ~> . todo-text)
    (λ (event text)
      (when (eq? event 'return)
        (action 'commit text))))
   (button
    "Delete"
    (λ () (action 'delete #f)))))

(render
 (window
  #:size '(800 600)
  (tabs
   '(all active done)
   #:choice->label (compose1 string-titlecase symbol->string)
   (lambda (event _choices selection)
     (case event
       [(select) (@tab . := . selection)]))
   (list-view
    @entries
    #:key todo-id
    (lambda (id @entry)
      (todo-view
       @entry
       (λ (event data)
         (define change-proc
           (case event
             [(check) (λ (ts) (update-todo ts id (λ (t) (struct-copy todo t [done? data]))))]
             [(commit) (λ (ts) (update-todo ts id (λ (t) (struct-copy todo t [text data]))))]
             [(delete) (λ (ts) (remove-todo ts id))]))
         (@todos . <~ . change-proc))))))
  (hpanel
   #:stretch '(#t #f)
   (button "Add first" (λ () (@todos . <~ . (λ (ts) (prepend-todo ts "New Item")))))
   (button "Add last"  (λ () (@todos . <~ . (λ (ts) (append-todo ts "New Item"))))))))
