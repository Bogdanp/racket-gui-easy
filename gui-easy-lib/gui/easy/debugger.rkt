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

(struct state (paused? max-changes changes))

(define (state-prepend-change s c)
  (define max-changes (state-max-changes s))
  (define changes (state-changes s))
  (struct-copy state s [changes (keep (cons c changes) max-changes)]))

(define (set-state-max-changes-str s num-str)
  (struct-copy state s [max-changes (or (string->number num-str)
                                        (state-max-changes s))]))

(define (toggle-state-paused? s)
  (struct-copy state s [paused? (not (state-paused? s))]))

(define (start-debugger)
  (define @state (@ (state #f 20 null)))
  (define stop-collector-thd
    (start-collector-thd
     (lambda (obs before after)
       (unless (or (equal? obs @state)
                   (obs-derived? obs))
         (@state . <~ . (λ (s)
                          (cond
                            [(state-paused? s) s]
                            [else
                             (define change (list (current-seconds) obs before after))
                             (state-prepend-change s change)])))))))
  (parameterize ([gui:current-eventspace (gui:make-eventspace)])
    (render
     (window
      #:title "Debugger"
      #:size '(400 600)
      #:mixin (make-debugger-window-mixin stop-collector-thd)
      (vpanel
       (hpanel
        #:stretch '(#t #f)
        (text "Keep:")
        (input
         (@state . ~> . (compose1 number->string state-max-changes))
         (λ (event text)
           (case event
             [(return)
              (@state . <~ . (λ (s) (set-state-max-changes-str s text)))])))
        (button
         (@state . ~> . (λ (s)
                          (if (state-paused? s)
                              "&Unpause..."
                              "&Pause...")))
         (λ ()
           (@state . <~ . toggle-state-paused?))))
       (table
        '("Timestamp" "Observable" "State")
        #:column-widths '((0 140)
                          (1 70)
                          (2 200))
        (@state . ~> . (compose1 list->vector state-changes))
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
             (obs . := . after)]))))))))

(define (~label s)
  (~e #:max-width 100 s))

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
