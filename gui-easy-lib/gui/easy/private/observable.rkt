#lang racket/base

(require (for-syntax racket/base)
         box-extra
         racket/struct
         version-case
         "executor.rkt"
         "logger.rkt")

(provide
 (rename-out [make-obs obs])
 impersonate-obs
 chaperone-obs
 ->obs
 obs?
 obs-name
 obs-rename
 obs-derived?
 obs-observe!
 obs-unobserve!
 obs-update!
 obs-set!
 obs-peek
 obs-map
 obs-filter
 obs-filter-map
 obs-combine
 obs-debounce
 obs-throttle)

;; We don't guarantee it anywhere, but values are dispatched to
;; observers in subscription order. In recent versions of Racket, we can
;; facilitate this by using treelists. In older versions, we use regular
;; lists and append new observers to the end of the list, assuming that
;; any given observable won't have enough observers to cause problems.
;;
;; Why not just keep a list and always reverse on update? Any given
;; observable is probably going to be updated more than it is going to
;; be subscribed to.
(version-case
 [(version>= (version) "8.12.0.7")
  (require racket/treelist)
  (define (make-observers)
    (treelist))
  (define (add-observer entries observer)
    (treelist-add entries observer))
  (define (remove-observer entries observer)
    (for/treelist ([v (in-treelist entries)]
                   #:unless (eq? v observer))
      v))
  (define (observers-empty? entries)
    (treelist-empty? entries))
  (define-syntax in-observers
    (make-rename-transformer #'in-treelist))]
 [else
  (define (make-observers) ;; noqa
    null)
  (define (add-observer entries observer) ;; noqa
    (append entries (list observer)))
  (define (remove-observer entries observer) ;; noqa
    (remq observer entries))
  (define (observers-empty? entries) ;; noqa
    (null? entries))
  (define-syntax in-observers
    (make-rename-transformer #'in-list))])

(struct obs
  ([name #:mutable]
   handle
   [value-box #:mutable] ; mutable only to allow impersonation on ref
   [update-value-box! #:mutable]
   observers-box
   update-observers-box!
   derived?)

  #:property prop:object-name
  (λ (this) (obs-name this))

  #:methods gen:equal+hash
  [(define (equal-proc o1 o2 _recursive-equal?)
     (and (obs? o1)
          (obs? o2)
          (eq? (obs-handle o1)
               (obs-handle o2))))

   (define (hash-proc o recursive-equal-hash)
     (recursive-equal-hash (obs-handle o)))

   (define (hash2-proc o recursive-equal-hash)
     (recursive-equal-hash (obs-handle o)))]

  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (_) 'obs)
      (λ (o) (list
              (unbox (obs-value-box o))
              (unquoted-printing-string "#:name")
              (obs-name o)
              (unquoted-printing-string "#:derived?")
              (obs-derived? o)))))])

(define (->obs v)
  (cond
    [(obs? v) v]
    [else (make-obs v)]))

(define (do-make-obs
         #:name [name 'anon]
         #:derived? [derived? #f]
         #:on-update-proc [on-update-hook void]
         v)
  (define handle (gensym name))
  (define value-box (box v))
  (define observers-box (box (make-observers)))
  (define update-value-box! (make-box-update-proc value-box))
  (define update-observers-box!
    (let ([proc (make-box-update-proc observers-box)])
      (lambda (update-entries)
        (proc
         (lambda (entries)
           (define updated-entries
             (update-entries entries))
           (on-update-hook updated-entries)
           updated-entries)))))
  (obs name
       handle
       value-box
       update-value-box!
       observers-box
       update-observers-box!
       derived?))

(define (make-obs v
                  #:name [name 'anon]
                  #:derived? [derived? #f])
  (do-make-obs
   #:name name
   #:derived? derived?
   v))

;; The os and the result of calling make-observers are parallel lists of
;; observable -> observer.
(define (make-derived-obs*
         who os
         #:name [name 'anon]
         #:init-value init-value
         #:make-observers-proc make-observers) ;; noqa
  (define derived
    (do-make-obs
     #:name name
     #:derived? #t
     #:on-update-proc
     (lambda (entries)
       (if (observers-empty? entries)
           (set-box! derived-box #f)
           (set-box! derived-box derived)))
     init-value))
  (define derived-box
    (box derived))
  (define (set!-value v)
    (define maybe-b (unbox derived-box))
    (when maybe-b
      (do-obs-update! maybe-b (λ (_) v))))
  (define observers
    (make-observers set!-value))
  (for ([o (in-list os)]
        [observer (in-list observers)])
    (obs-observe! o observer))
  (will-register
   executor derived
   (lambda (_)
     (for ([o (in-list os)]
           [observer (in-list observers)])
       (log-gui-easy-debug "~a: unobserved ~.s" who observer)
       (obs-unobserve! o observer))))
  derived)

;; Like make-derived-obs*, but for a single observable -> observer pair.
(define (make-derived-obs
         who a
         #:name [name 'anon]
         #:init-value [init-value (obs-peek a)]
         #:make-observer-proc [make-observer (λ (_set!-value) null)]) ;; noqa
  (make-derived-obs*
   who (list a)
   #:name name
   #:init-value init-value
   #:make-observers-proc
   (lambda (set!-value)
     (list (make-observer set!-value)))))

(define (do-impersonate-obs o
                            do-impersonate-box
                            do-impersonate-procedure
                            val-ref
                            val-set)
  (impersonate-struct o
                      obs-value-box
                      (and val-ref
                           (lambda (o bx) ;; noqa
                             (do-impersonate-box
                              bx
                              (lambda (_ v) (val-ref o v))
                              (lambda (_ v) v))))
                      set-obs-value-box!
                      #f ; witness
                      obs-update-value-box!
                      (and val-set
                           (λ (o update-proc) ; update-proc: (a -> b) -> b ;; noqa
                             (do-impersonate-procedure
                              update-proc ; update-proc': (a -> b) -> c
                              (λ (proc) ; proc: a -> b
                                (λ (v) ; proc': a -> c
                                  ; val-set: b -> c
                                  (val-set o (proc v)))))))
                      set-obs-update-value-box!!
                      #f))

(define (chaperone-obs o
                       #:ref [val-ref #f]
                       #:set [val-set #f])
  (do-impersonate-obs o
                      chaperone-box
                      chaperone-procedure
                      val-ref
                      val-set))

(define (impersonate-obs o
                         #:ref [val-ref #f]
                         #:set [val-set #f])
  (do-impersonate-obs o
                      impersonate-box
                      impersonate-procedure
                      val-ref
                      val-set))

(define (obs-rename o name)
  (impersonate-struct o
                      obs-name
                      (λ (_ _name) name)
                      set-obs-name!
                      #f))

(define (obs-observe! o observer)
  (void
   ((obs-update-observers-box! o)
    (lambda (entries)
      (add-observer entries observer)))))

(define (obs-unobserve! o observer)
  (void
   ((obs-update-observers-box! o)
    (lambda (entries)
      (remove-observer entries observer)))))

(define (do-obs-update! o proc)
  (define old-v (unbox (obs-value-box o)))
  (define updated-v ((obs-update-value-box! o) proc))
  (log-change o old-v updated-v)
  (define observers (unbox (obs-observers-box o)))
  (for ([observer (in-observers observers)])
    (with-handlers ([exn:fail?
                     (lambda (e)
                       ((error-display-handler)
                        (format "do-obs-update!: ~a" (exn-message e))
                        e))])
      (observer updated-v)))
  updated-v)

(define (obs-update! o f)
  (when (obs-derived? o)
    (raise-argument-error 'obs-update! "(not/c obs-derived?)" o))
  (do-obs-update! o f))

(define (obs-set! o v)
  (void (obs-update! o (λ (_) v))))

(define (obs-peek o)
  (unbox (obs-value-box o)))

(define (obs-map a proc)
  (make-derived-obs
   'obs-map a
   #:init-value (proc (obs-peek a))
   #:make-observer-proc
   (lambda (set!-value)
     (lambda (v)
       (set!-value (proc v))))))

(define (obs-filter a proc [default #f])
  (obs-filter-map a (λ (v) (and (proc v) v)) default))

(define (obs-filter-map a proc [default #f])
  (make-derived-obs
   'obs-filter-map a
   #:init-value (or (proc (obs-peek a)) default)
   #:make-observer-proc
   (lambda (set!-value)
     (lambda (v)
       (define updated-v (proc v))
       (when updated-v
         (set!-value updated-v))))))

(define (obs-combine proc . os)
  (define vals
    (for/vector
        #:length (length os)
        ([o (in-list os)])
      (obs-peek o)))
  (make-derived-obs*
   'obs-combine os
   #:init-value (apply proc (vector->list vals))
   #:make-observers-proc
   (lambda (set!-value)
     (for/list ([_ (in-list os)]
                [i (in-naturals)])
       (lambda (v)
         (vector-set! vals i v)
         (set!-value (apply proc (vector->list vals))))))))

(define nothing (string->uninterned-symbol "nothing"))

(define (obs-debounce a #:duration [duration 200])
  (make-derived-obs
   'obs-debounce a
   #:init-value (obs-peek a)
   #:make-observer-proc
   (lambda (set!-value)
     (define ch (make-channel))
     (thread
      (lambda ()
        (let loop ([pending nothing])
          (sync
           (handle-evt ch loop)
           (if (eq? pending nothing)
               never-evt
               (handle-evt
                (alarm-evt
                 #;msecs (+ (current-inexact-monotonic-milliseconds) duration)
                 #;monotonic? #t)
                (lambda (_)
                  (set!-value pending)
                  (loop nothing))))))))
     (lambda (v)
       (channel-put ch v)))))

(define (obs-throttle a #:duration [duration 200])
  (make-derived-obs
   'obs-throttle a
   #:init-value (obs-peek a)
   #:make-observer-proc
   (lambda (set!-value)
     (define ch (make-channel))
     (thread
      (lambda ()
        (let loop ([pending nothing]
                   [pending-alarm #f])
          (sync
           (handle-evt
            ch
            (lambda (pending-v)
              (loop
               pending-v
               (or pending-alarm
                   (alarm-evt
                    #;msecs (+ (current-inexact-monotonic-milliseconds) duration)
                    #;monotonic? #t)))))
           (if (eq? pending nothing)
               never-evt
               (handle-evt
                pending-alarm
                (lambda (_)
                  (set!-value pending)
                  (loop nothing #f))))))))
     (lambda (v)
       (channel-put ch v)))))

(module+ test
  (require racket/port
           rackunit
           (submod ".."))

  (define @a (obs 1))
  (check-equal? (obs-peek @a) 1)
  (check-equal? (obs-update! @a add1) 2)
  (check-equal? (obs-peek @a) 2)

  (define @b (obs-map @a number->string))
  (check-equal? (obs-peek @b) "2")
  (check-exn
   #rx"not/c obs-derived"
   (λ () (obs-update! @b "3")))
  (check-equal? (obs-peek @b) "2")
  (check-equal? (obs-update! @a add1) 3)
  (check-equal? (obs-peek @b) "3")

  (define @c (obs 10))
  (define @d (obs-combine list @a @b @c))
  (check-equal? (obs-peek @d) (list 3 "3" 10))
  (obs-update! @a add1)
  (check-equal? (obs-peek @d) (list 4 "4" 10))
  (obs-update! @c add1)
  (check-equal? (obs-peek @d) (list 4 "4" 11))

  (define @evens (obs-filter @a even?))
  (define @odds (obs-filter @a odd?))
  (check-equal? (obs-peek @evens) 4)
  (check-equal? (obs-peek @odds) #f)
  (obs-update! @a add1)
  (check-equal? (obs-peek @evens) 4)
  (check-equal? (obs-peek @odds) 5)
  (obs-update! @a add1)
  (check-equal? (obs-peek @evens) 6)
  (check-equal? (obs-peek @odds) 5)
  (obs-update! @a add1)

  (test-case "obs-map gc"
    (define b #f)
    (define @a (obs 1)) ;; noqa
    (define @b (obs-map @a add1)) ;; noqa
    (define (set!-b v)
      (set! b v))
    (obs-observe! @b set!-b)
    (obs-update! @a add1)
    (sync (system-idle-evt))
    (check-equal? b 3)
    (obs-unobserve! @b set!-b)
    (set! @b #f)
    (collect-garbage)
    (collect-garbage)
    (sync (system-idle-evt))
    (obs-update! @a add1)
    (sync (system-idle-evt))
    (check-equal? b 3))

  (test-case "observable names"
    (define @foo (obs 42 #:name '@foo))
    (check-equal? (object-name @foo) '@foo))

  (test-case "unobserve"
    (define x #f)
    (define (set-x! v)
      (set! x v))
    (define @obs (obs #f))
    (obs-observe! @obs set-x!)
    (obs-update! @obs (λ (_) 10))
    (sync (system-idle-evt))
    (check-equal? x 10)
    (obs-unobserve! @obs set-x!)
    (obs-update! @obs (λ (_) 42))
    (sync (system-idle-evt))
    (check-equal? x 10))

  (test-case "observer exn"
    (define x #f)
    (define y #f)
    (define @obs (obs #f))
    (obs-observe! @obs (λ (_) (error 'x "failed")))
    (obs-observe! @obs (λ (v) (set! y v)))
    (parameterize ([current-error-port (open-output-nowhere)])
      (obs-update! @obs (λ (_) 42)))
    (sync (system-idle-evt))
    (check-false x)
    (check-equal? y 42))

  (test-case "obs-debounce"
    (define xs null)
    (define @a (obs 1))
    (define @b (obs-debounce @a))
    (define sema (make-semaphore))
    (define (observer x)
      (set! xs (cons x xs))
      (semaphore-post sema))
    (obs-observe! @b observer)
    (for ([_ (in-range 10)])
      (obs-update! @a add1))
    (sync (system-idle-evt))
    (sync sema)
    (check-equal? xs '(11))
    (obs-unobserve! @b observer)
    (set! @b #f)
    (sync (system-idle-evt))
    (collect-garbage)
    (collect-garbage)
    (obs-update! @a add1)
    (sync (system-idle-evt))
    (sync (alarm-evt (+ (current-inexact-monotonic-milliseconds) 350) #;monotonic? #t))
    (sync (system-idle-evt))
    (check-equal? xs '(11)))

  (test-case "obs-throttle"
    (define xs null)
    (define @a (obs 1))
    (define @b (obs-throttle @a))
    (define sema (make-semaphore))
    (define (observer x)
      (set! xs (cons x xs))
      (semaphore-post sema))
    (obs-observe! @b observer)
    (for ([_ (in-range 10)])
      (obs-update! @a add1))
    (sync (system-idle-evt))
    (sync sema)
    (check-equal? xs '(11))
    (obs-unobserve! @b observer)
    (set! @b #f)
    (sync (system-idle-evt))
    (collect-garbage)
    (collect-garbage)
    (obs-update! @a add1)
    (sync (system-idle-evt))
    (sync (alarm-evt (+ (current-inexact-monotonic-milliseconds) 350) #;monotonic? #t))
    (sync (system-idle-evt))
    (check-equal? xs '(11))))
