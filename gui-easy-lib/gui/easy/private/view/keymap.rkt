#lang racket/base

(require (for-syntax racket/base)
         racket/class
         (prefix-in gui: racket/gui))

(provide
 the-default-keymap

 backward-char
 backward-delete-char
 backward-delete-word
 backward-select-word
 backward-word
 delete-char
 forward-char
 forward-select-word
 forward-word
 goto-end
 goto-start
 next-line
 previous-line)

(define the-default-keymap
  (new gui:keymap%))

(define ((make-editor-operation id . args) editor _event)
  (apply dynamic-send editor id args))

(define (goto-start editor _event)
  (define pos (send editor get-start-position))
  (send editor set-position (or (send editor find-newline 'backward pos 0) 0)))

(define (goto-end editor _event)
  (define pos (send editor get-end-position))
  (define target-pos (send editor find-newline 'forward pos))
  (define last-pos (send editor last-position))
  (send editor set-position (if (or (not target-pos)
                                    (= target-pos last-pos))
                                last-pos
                                (sub1 target-pos))))

(define (find-wordbreak editor start end)
  (define sb (and start (box (if (eq? start 'ask) (send editor get-start-position) start))))
  (define eb (and end   (box (if (eq? end   'ask) (send editor get-end-position)   end))) )
  (send editor find-wordbreak sb eb 'caret)
  (values (and sb (unbox sb))
          (and eb (unbox eb))))

(define (forward-word editor _event)
  (define-values (_ pos)
    (find-wordbreak editor #f 'ask))
  (send editor set-position pos))

(define (forward-select-word editor _event)
  (define start-pos (send editor get-start-position))
  (define-values (_ end-pos)
    (find-wordbreak editor #f 'ask))
  (send editor set-position start-pos end-pos))

(define (backward-word editor _event)
  (define-values (pos _)
    (find-wordbreak editor 'ask #f))
  (send editor set-position pos))

(define (backward-select-word editor _event)
  (define-values (start-pos _)
    (find-wordbreak editor 'ask #f))
  (define end-pos (send editor get-end-position))
  (send editor set-position start-pos end-pos))

(define (backward-delete-word editor _event)
  (define start-pos (send editor get-start-position))
  (define-values (end-pos _)
    (find-wordbreak editor 'ask #f))
  (unless (= start-pos end-pos)
    (send editor delete end-pos start-pos)))

(define ((make-char-proc proc) editor _event)
  (define pos (send editor get-start-position))
  (define last-pos (send editor last-position))
  (define target-pos (max 0 (min (proc pos) last-pos)))
  (send editor set-position target-pos))

(define backward-char (make-char-proc sub1))
(define forward-char (make-char-proc add1))

(define (backward-delete-char editor _event)
  (send editor delete 'start))

(define (delete-char editor _event)
  (define position (send editor get-start-position))
  (send editor delete position (add1 position)))

(define ((make-line-proc proc) editor _event)
  (define pos (send editor get-start-position))
  (define line (send editor position-line pos))
  (define line-start-pos (send editor line-start-position line))
  (define line-offset (- pos line-start-pos))
  (define last-line (send editor last-line))
  (define target-line (min (max 0 (proc line)) last-line))
  (define target-line-start-pos (send editor line-start-position target-line))
  (define target-line-end-pos (send editor line-end-position target-line))
  (define target-position (min target-line-end-pos (+ target-line-start-pos line-offset)))
  (send editor set-position target-position))

(define previous-line (make-line-proc sub1))
(define next-line (make-line-proc add1))

(define (kill-line editor _event)
  (send editor kill (current-seconds)))

(define (paste editor _event)
  (send editor paste))

(define-syntax (define-proc stx)
  (syntax-case stx ()
    [(_ id) #'(define-proc id id)]
    [(_ id e) #'(send the-default-keymap add-function (symbol->string 'id) e)]))

(define-syntax-rule (define-procs binder ...)
  (begin (define-proc . binder) ...))

(define-syntax-rule (bind [binding id] ...)
  (begin (send the-default-keymap map-function binding (symbol->string 'id)) ...))

(define-procs
  [backward-char]
  [backward-delete-char]
  [backward-delete-word]
  [backward-select-word]
  [backward-word]
  [copy (make-editor-operation 'copy)]
  [cut (make-editor-operation 'cut)]
  [delete-char]
  [forward-char]
  [forward-select-word]
  [forward-word]
  [goto-end]
  [goto-start]
  [kill-line]
  [next-line]
  [paste (make-editor-operation 'paste)]
  [previous-line]
  [redo (make-editor-operation 'redo)]
  [select-all (make-editor-operation 'select-all)]
  [undo (make-editor-operation 'undo)])

(case (system-type 'os)
  [(macosx)
   (bind
    ["a:b"         backward-word]
    ["a:backspace" backward-delete-word]
    ["a:f"         forward-word]
    ["a:left"      backward-word]
    ["a:right"     forward-word]
    ["a:s:left"    backward-select-word]
    ["a:s:right"   forward-select-word]
    ["c:a"         goto-start]
    ["c:b"         backward-char]
    ["c:d"         delete-char]
    ["c:e"         goto-end]
    ["c:f"         forward-char]
    ["c:k"         kill-line]
    ["c:n"         next-line]
    ["c:p"         previous-line]
    ["c:s:d"       backward-delete-char]
    ["c:w"         backward-delete-word]
    ["c:y"         paste]
    ["d:Z"         redo]
    ["d:a"         select-all]
    ["d:c"         copy]
    ["d:v"         paste]
    ["d:x"         cut]
    ["d:z"         undo])]
  [else
   (bind
    ["c:Z"         redo]
    ["c:a"         select-all]
    ["c:backspace" backward-delete-word]
    ["c:c"         copy]
    ["c:left"      backward-word]
    ["c:n"         next-line]
    ["c:p"         previous-line]
    ["c:right"     forward-word]
    ["c:v"         paste]
    ["c:x"         cut]
    ["c:z"         undo]
    ["end"         goto-end]
    ["home"        goto-start])])
