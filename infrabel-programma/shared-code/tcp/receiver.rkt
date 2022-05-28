#lang racket

(provide maak-receiver)

(define (maak-receiver listen-port receiver-dispatch)
  (let* ((tcp-conn (tcp-listen listen-port 5)))

    

    (define (check-inkomende-berichten!)
      (define-values (in out) (tcp-accept tcp-conn))
      (handle in out)
      (close-input-port in)
      (close-output-port out))

    (define (handle in out)
      (let ((info (read in)))
        (unless (eof-object? info)
          (let ((res  (let ((msg (car info))
                            (args (cdr info)))
                        (apply (receiver-dispatch msg) args))))
            (unless (void? res)
              (write res out)
              (flush-output out))))))

    (define (sluit-connectie)
      (tcp-close tcp-conn))


    (define (dispatch msg)
      (cond ((eq? msg 'check-inkomende-berichten!) check-inkomende-berichten!)
            ((eq? msg 'sluit-connectie) sluit-connectie)
            (else (error "ongekend bericht" msg))))
    
    dispatch))