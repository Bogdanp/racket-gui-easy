#lang racket/base

(require (prefix-in gui: racket/gui)
         racket/gui/easy
         racket/gui/easy/operator
         racket/port)

(struct buffer (path contents) #:transparent)

(define (buffer-name b)
  (define p (buffer-path b))
  (define-values (_base filename _must-be-dir?)
    (split-path p))
  (path->string filename))

(define @buffers (@ null))
(define @buffer-index (@ #f))
(define @current-buffer
  (obs-debounce
   #:duration 100
   (obs-combine
    (λ (buffers index)
      (and index
           (< index (length buffers))
           (list-ref buffers index)))
    @buffers @buffer-index)))

(define (remove-at-index xs idx)
  (for/list ([x (in-list xs)]
             [i (in-naturals)]
             #:unless (= i idx))
    x))

(render
 (window
  #:size '(600 600)
  (button
   "Open file..."
   (λ ()
     (define filename (gui:get-file))
     (when filename
       (thread
        (lambda ()
          (define contents
            (call-with-input-file filename port->string))
          (@buffers . <~ . (λ (buffers)
                             (append buffers (list (buffer filename contents))))))))))
  (tabs
   #:style '(no-border can-close can-reorder)
   #:choice->label buffer-name
   #:selection @buffer-index
   @buffers
   (let ([ignore-select? #f])
     (λ (event buffers index)
       (case event
         [(close)
          (when (= (obs-peek @buffer-index) index)
            (@buffer-index . := . (max 0 (sub1 index))))
          (@buffers . := . (remove-at-index buffers index))]

         [(reorder)
          (set! ignore-select? #t)
          (@buffers . := . buffers)
          (@buffer-index . := . index)]

         [(select)
          (unless ignore-select?
            (@buffer-index . := . index))
          (set! ignore-select? #f)])))
   (input
    #:font (font "Operator Mono" 12 #:weight 'light #:family 'modern)
    #:style '(multiple)
    #:margin '(0 0)
    (@current-buffer . ~> . (λ (b)
                              (cond
                                [b (buffer-contents b)]
                                [else ""])))))))
