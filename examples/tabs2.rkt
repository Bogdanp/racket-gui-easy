#lang racket/base

(require racket/format
         racket/gui/easy
         racket/gui/easy/operator
         racket/list
         racket/match)

(struct person (id name age) #:transparent)

(define (~person p)
  (match-define (person _ name age) p)
  (if age
      (format "~a (~a)" name age)
      name))

(define (~person-age p)
  (cond
    [(person-age p) => ~a]
    [else ""]))

(define (person-detail p [action void])
  (define/obs @person p)
  (vpanel
   (hpanel
    #:stretch '(#t #f)
    (text "Name:")
    (input
     (person-name p)
     (λ (_event text)
       (@person . <~ . (λ (p) (struct-copy person p [name text]))))))
   (hpanel
    #:stretch '(#t #f)
    (text "Age:")
    (input
     (~person-age p)
     (λ (_event text)
       (define maybe-age (string->number text))
       (when maybe-age
         (@person . <~ . (λ (p) (struct-copy person p [age maybe-age])))))))
   (hpanel
    #:stretch '(#t #f)
    #:alignment '(right center)
    (button "Save" (λ () (action (obs-peek @person)))))))

(struct state (people selection) #:transparent)

(define (state-current-person st)
  (match-define (state people selection) st)
  (findf (λ (p) (= (person-id p) selection)) people))

(define (state-current-person-index st)
  (match-define (state people selection) st)
  (index-where people (λ (p) (= (person-id p) selection))))

(define (state-add-person st)
  (match-define (state people _) st)
  (define next-id (add1 (apply max (map person-id people))))
  (define new-people (append people (list (person next-id "Unnamed" #f))))
  (struct-copy state st
               [people new-people]
               [selection next-id]))

(define (state-remove-person st id)
  (match-define (state people selection) st)
  (cond
    [(= (length people) 1) st]
    [(eqv? selection id)
     (define new-people (remf (λ (p) (= id (person-id p))) people))
     (struct-copy state st
                  [people new-people]
                  [selection (person-id (last new-people))])]
    [else
     (struct-copy state st
                  [people (remf (λ (p) (= id (person-id p))) people)])]))

(define (state-update-person st id proc)
  (match-define (state people _) st)
  (define new-people
    (for/list ([p (in-list people)])
      (if (eqv? (person-id p) id)
          (proc p)
          p)))
  (struct-copy state st [people new-people]))

(define ((make-person-selector p) st)
  (struct-copy state st [selection (person-id p)]))

(define ((make-person-setter p) st)
  (state-update-person st (person-id p) (λ (_) p)))

(define ((make-person-remover p) st)
  (state-remove-person st (person-id p)))

(define/obs @state
  (state (list (person 0 "Bogdan" 30)) 0))

(render
 (window
  #:size '(800 600)
  (tabs
   (@state . ~> . state-people)
   #:style '(no-border can-reorder can-close new-button flat-portable)
   #:choice->label ~person
   #:selection (@state . ~> . state-current-person-index)
   (lambda (event people selection)
     (case event
       [(new)
        (@state . <~ . state-add-person)]
       [(select)
        (@state . <~ . (make-person-selector (list-ref people selection)))]
       [(reorder)
        (@state . <~ . (λ (st) (struct-copy state st [people people])))]
       [(close)
        (@state . <~ . (make-person-remover (list-ref people selection)))]))
   (dyn-view
    (@state . ~> . state-current-person)
    (λ (p)
      (person-detail p (λ (p) (@state . <~ . (make-person-setter p)))))))))
