#lang racket/base

(provide
 executor)

(define executor
  (make-will-executor))

(void
 (parameterize ([current-namespace (make-empty-namespace)])
   (thread
    (lambda ()
      (let loop ()
        (with-handlers ([exn:fail?
                         (lambda (e)
                           ((error-display-handler) (exn-message e) e))])
          (will-execute executor))
        (loop))))))
