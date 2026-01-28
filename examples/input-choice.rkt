#lang racket/gui/easy

(define workday-hours '("08:00" "10:00" "12:00" "14:00" "16:00"))
(define extended-hours (append '("04:00" "06:00")
                               workday-hours
                               '("18:00" "20:00")))

(define/obs @suggested-hours workday-hours)
(define/obs @validated "")

(render
 (window
  ;; different times sugegsted for different levels of service
  (radios '("Standard service"
            "Extended service")
          (lambda (v)
            (obs-set! @suggested-hours
                      (if (equal? v "Standard service")
                          workday-hours
                          extended-hours))))
  (input #:label "Appointment:"
         (car workday-hours)
         #:choices @suggested-hours
         (lambda (_ str)
           (obs-set! @validated (if (and (regexp-match? #rx"^[0-9][0-9]:[0-9][0-9]$" str)
                                         (and (<= 0 (string->number (substring str 0 2)) 23)
                                              (<= 0 (string->number (substring str 3 5)) 59)))
                                    (format "~a if available" str)
                                    "invalid time"))))
  (text @validated)))
