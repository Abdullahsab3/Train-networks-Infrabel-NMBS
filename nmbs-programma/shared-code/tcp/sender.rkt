#lang racket
(require racket/tcp)
(provide maak-sender)


(define (maak-sender ip receiver-port)

  (define (send info)
    (define-values (in out) (tcp-connect ip receiver-port))
    (write info out)
    (flush-output out)
    (close-output-port out)
    (let ((info (let ((inkomend (read in)))
                  (if (eof-object? inkomend)
                      'geen-inkomend
                      inkomend))))
      (close-input-port in)

      info))

  (define (get-ip-adres)
    (define-values (in out) (tcp-connect ip receiver-port))
    (define-values (myip yourip) (tcp-addresses in))
    (close-output-port out)
    (close-input-port in)
    myip)

  (define (dispatch msg)
    (cond ((eq? msg 'send) send)
          ((eq? msg 'get-ip-adres) get-ip-adres)
          (else (error "ongekend bericht" msg))))
  dispatch)