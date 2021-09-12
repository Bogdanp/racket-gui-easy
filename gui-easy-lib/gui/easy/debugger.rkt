#lang racket/base

(require racket/class
         racket/date
         racket/format
         (prefix-in gui: racket/gui)
         racket/list
         racket/match
         "operator.rkt"
         "private/logger.rkt"
         "private/observable.rkt"
         "renderer.rkt"
         "view.rkt")

(provide
 start-debugger)

(define (keep xs n)
  (take xs (min n (length xs))))

(define ((make-debugger-window-mixin on-close-proc) %)
  (class %
    (super-new)
    (define/augment (on-close)
      (on-close-proc))))

(define (start-collector-thd on-change)
  (define stop-ch (make-channel))
  (thread
   (lambda ()
     (define change-evt (make-change-evt))
     (let loop ()
       (sync
        (handle-evt stop-ch void)
        (handle-evt
         change-evt
         (λ (obs before after)
           (on-change obs before after)
           (loop)))))))
  (λ ()
    (channel-put stop-ch #t)))

(define (start-debugger)
  (define @state (@ null))
  (define stop-collector-thd
    (start-collector-thd
     (lambda (obs before after)
       (unless (or (equal? obs @state)
                   (obs-derived? obs))
         (@state . <~ . (λ (state)
                          (keep (cons (list (current-seconds) obs before after) state) 100)))))))
  (parameterize ([gui:current-eventspace (gui:make-eventspace)])
    (render
     (window
      #:title "Debugger"
      #:size '(400 600)
      #:mixin (make-debugger-window-mixin stop-collector-thd)
      (table
       '("Timestamp" "Observable" "State")
       #:column-widths '((0 140)
                         (1 70)
                         (2 200))
       (@state . ~> . list->vector)
       #:entry->row (λ (entry)
                      (match-define (list ts obs _before after) entry)
                      (vector
                       (parameterize ([date-display-format 'iso-8601])
                         (date->string (seconds->date ts) #t))
                       (~a (obs-name obs))
                       (~label after)))
       (lambda (event entries selection)
         (case event
           [(dclick)
            (match-define (list _ts obs _before after)
              (vector-ref entries selection) )
            (obs . := . after)])))))))

(define (~label s)
  (~e #:max-width 100 s))
