#lang racket

(provide make-timer)


;;;; een timer ADT ;;;;

;; uit mijn eindproject voor Besturingssytemen en systeemfundamenten. Goede tijden :D
(define (make-timer)
  (let ((current-time (current-seconds))
        (how-long #f))

    
    ;; de huidige tijd van de timer resetten
    (define (reset-time!)
       (set! current-time (current-seconds)))
       

    ;; kijken of de tijd van de timer al gepasseerd is
    (define (time-passed?)
      (let* ((time-now  (current-seconds))
             (passed? (>=  (- time-now current-time) how-long)))
        ;; als de tijd al gepasseerd is, resetten we de timer zodat die opnieuw kan timen.
        (when passed?
          (reset-time!))
        passed?))


    (define (dispatch msg)
      (cond ((eq? msg 'time-passed?) (time-passed?))
            ((eq? msg 'start!) (lambda (duur)
                                 (set! how-long duur)
                                 (reset-time!)))
            (else (error "unknown message" msg))))
    dispatch))
