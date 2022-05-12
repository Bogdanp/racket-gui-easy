#lang racket/base

(require racket/class
         racket/contract
         racket/draw
         (prefix-in gui: racket/gui)
         racket/string)

(provide
 (contract-out
  [font (font/c string? (is-a?/c gui:font%))]
  [font* (font/c (or/c string? (listof string?)) (or/c #f (is-a?/c gui:font%)))]))

(define (font/c name/c res/c)
  (->* (name/c (real-in 0 1024.0))
       (#:family gui:font-family/c
        #:style gui:font-style/c
        #:weight gui:font-weight/c
        #:underline? any/c
        #:smoothing gui:font-smoothing/c
        #:hinting gui:font-hinting/c
        #:size-in-pixels? any/c)
       res/c))

(define (font face size
              #:family [family 'default]
              #:style [style 'normal]
              #:weight [weight 'normal]
              #:underline? [underline? #f]
              #:smoothing [smoothing 'default]
              #:hinting [hinting 'aligned]
              #:size-in-pixels? [pixels? #f])
  (send gui:the-font-list find-or-create-font size face family style weight underline? smoothing pixels? hinting))

(define font*
  (make-keyword-procedure
   (lambda (kws kw-args faces size)
     (let ([faces (if (string? faces)
                      (map string-trim (string-split faces ","))
                      faces)])
       (define installed-faces (map string-downcase (get-face-list)))
       (for/first ([face (in-list faces)]
                   #:when (member (string-downcase face) installed-faces string=?))
         (keyword-apply font kws kw-args face size null))))))
