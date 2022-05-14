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

(define/obs @buffers null)
(define/obs @buffer #f)

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
                             (define buf (buffer filename contents))
                             (begin0 (append buffers (list buf))
                               (@buffer . := . buf)))))))))
  (tabs
   #:style '(no-border can-close can-reorder)
   #:selection @buffer
   #:choice->label buffer-name
   #:choice=? eq?
   @buffers
   (λ (event bs b)
     (case event
       [(close)
        (@buffers . := . bs)
        (@buffer . := . b)]

       [(reorder)
        (@buffers . := . bs)]

       [(select)
        (@buffers . := . bs)
        (@buffer . := . b)]))
   (input
    #:font (font "Operator Mono" 12 #:weight 'light #:family 'modern)
    #:style '(multiple)
    #:margin '(0 0)
    #:stretch '(#t #t)
    (@buffer . ~> . (λ (b)
                      (cond
                        [b (buffer-contents b)]
                        [else ""])))))))
