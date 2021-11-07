#lang racket/base

(require (prefix-in p: pict)
         racket/class
         racket/format
         (prefix-in gui: racket/gui)
         racket/gui/easy
         racket/gui/easy/operator
         racket/list
         racket/match
         racket/port
         racket/string)


;; cell ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(define index (string->list alphabet))

(define cell-width 100)
(define cell-height 30)

(struct cell (formula content))

(define (make-cell-name row col)
  (string->symbol (~cell-name row col)))

(define (~cell-name row col)
  (~a (string-ref alphabet col) row))

(define (~cell c)
  (cond
    [(cell-formula c)
     => (λ (e)
          (call-with-output-string
           (lambda (out)
             (write-char #\= out)
             (write e out))))]
    [else (cell-content c)]))

(define (cell-pict c)
  (if (string=? (cell-content c) "")
      (p:rectangle cell-width cell-height)
      (p:clip
       (struct-copy p:pict
                    (p:lc-superimpose
                     (p:rectangle cell-width cell-height)
                     (p:inset (p:text (cell-content c)) 5 0))
                    [width cell-width]))))

(define (table-pict rows)
  (apply
   p:vl-append
   (for/list ([r (in-vector rows)])
     (apply
      p:ht-append
      (for/list ([c (in-vector r)])
        (cell-pict c))))))


;; formula ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ops
  (hasheq
   '+ +
   '- -
   '* *
   '/ /))

(define (parse-formula s)
  (with-handlers ([(λ (_) #t)
                   (λ (_) #f)])
    (call-with-input-string s read)))

(define (formula-deps e)
  (remove-duplicates
   (match e
     [(? symbol? id)
      #:when (valid-cell-name? id)
      (list id)]

     [`(,(? symbol? op) ,e0 ,e1)
      #:when (hash-has-key? ops op)
      (append (formula-deps e0)
              (formula-deps e1))]

     [_
      null])))

(define (eval-formula s e)
  (number->string
   (let help ([e e])
     (match e
       [(? number?) e]

       [(? symbol? id)
        #:when (valid-cell-name? id)
        (define-values (row col)
          (parse-cell-name id))
        (state-ref-num s row col)]

       [`(,(? symbol? op) ,e0 ,e1)
        #:when (hash-has-key? ops op)
        ((hash-ref ops op)
         (help e0)
         (help e1))]

       [_ 0]))))


;; state ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; deps: map of cell-name -> dependent cell-name
;; cells: (vectorof (vectorof cell?))
(struct state (deps cells))

(define/obs @state
  (state (hasheq) (for/vector ([_ (in-range 100)])
                    (for*/vector ([_ (in-range 26)])
                      (cell #f "")))))

(define (state-ref s row col)
  (vector-ref (vector-ref (state-cells s) row) col))

(define (state-ref-num s row col)
  (or (string->number (cell-content (state-ref s row col))) 0))

(define (track-cell-depends s cell-name depends)
  (define deps-removed
    (for/hash ([(cell ds) (in-hash (state-deps s))])
      (values cell (remq cell-name ds))))
  (define deps-added
    (for/fold ([res deps-removed])
              ([d (in-list depends)])
      (hash-update res d (λ (ds) (cons cell-name ds)) null)))
  (struct-copy state s [deps deps-added]))

(define (set-cell! s row col the-cell)
  (vector-set! (vector-ref (state-cells s) row) col the-cell))

(define (change-cell! s row col content)
  (define cell-name (make-cell-name row col))
  (define e (and (string-prefix? content "=")
                 (parse-formula (substring content 1))))
  (define c
    (cell e content))
  (define new-s
    (track-cell-depends s cell-name (if e (formula-deps e) null)))
  (begin0 new-s
    (set-cell! new-s row col c)
    (update-cell! new-s row col)))

(define (update-cell! s row col)
  (define cell-name (make-cell-name row col))
  (define the-cell (vector-ref (vector-ref (state-cells s) row) col))
  (define new-cell
    (struct-copy cell the-cell
                 [content (cond
                            [(cell-formula the-cell)
                             => (λ (e)
                                  (eval-formula s e))]
                            [else
                             (cell-content the-cell)])]))
  (set-cell! s row col new-cell)
  (for ([dep (in-list (reverse (hash-ref (state-deps s) cell-name null)))])
    (define-values (dep-row dep-col)
      (parse-cell-name dep))
    (update-cell! s dep-row dep-col)))

(define (parse-cell-name id)
  (match (symbol->string id)
    [(regexp #rx"^([A-Z])(0|[1-9][0-9]|[1-9])$" `(,_ ,col-name ,row))
     (values
      (string->number row)
      (index-of index (string-ref col-name 0)))]

    [_
     (values #f #f)]))

(define (valid-cell-name? id)
  (define-values (row col)
    (parse-cell-name id))
  (and row col))


;; GUI ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define r
  (render
   (window
    #:title "Cells"
    #:size '(800 600)
    (pict-canvas
     (@state . ~> . state-cells)
     #:style '(hscroll vscroll)
     #:mixin (λ (%)
               (class %
                 (inherit get-view-start init-auto-scrollbars)
                 (super-new)
                 (init-auto-scrollbars
                  (* cell-width 26)
                  (* 100 cell-height)
                  0 0)

                 (define last-click-ts 0)
                 (define/override (on-event e)
                   (define x (send e get-x))
                   (define y (send e get-y))
                   (case (send e get-event-type)
                     [(left-up)
                      (when (< (- (send e get-time-stamp) last-click-ts) 250)
                        (define-values (cx cy)
                          (get-view-start))
                        (define ax (+ cx x))
                        (define ay (+ cy y))
                        (define row (quotient ay 30))
                        (define col (quotient ax 100))
                        (render-cell-changer r row col))
                      (set! last-click-ts (send e get-time-stamp))]))))
     table-pict))))

(define (render-cell-changer parent row col)
  (render
   (dialog
    #:title (~cell-name row col)
    (input
     #:min-size '(200 #f)
     (~cell (state-ref (obs-peek @state) row col))
     (λ (event text)
       (case event
         [(input)
          (@state . <~ . (λ (s)
                           (change-cell! s row col text)))]
         [(return)
          ((gui:application-quit-handler))]))))
   parent))
