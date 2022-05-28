#lang racket
(require "DNS-resolver.rkt")

(provide get-ip-address
         get-pi-ip
         check-ipv4)

(define (get-pi-ip)
  (resolve-DNS 'raspberrypi.local))

(define (execute-command-with-string-out command)
  (let* ((cmd (process command))
         (in (car cmd))
         (out (cadr cmd))
         (err (cadddr cmd)))
    
    (define info "")
    
    (let loop ((inkomend (read-line in)))
      (unless (eof-object? inkomend)
        (set! info (~a inkomend info))
        (loop (read-line in))))
        
    (close-output-port out)
    (close-input-port in)
    (close-input-port err)
    info))

(define (find-ipv4-in-string str)
  (regexp-match #px"(?:[0-9]{1,3}\\.){3}[0-9]{1,3}" str))

(define (check-ipv4 addr)
  (regexp-match #px"^(?:[0-9]{1,3}\\.){3}[0-9]{1,3}$" addr))


(define (get-ip-address-windows)
  (let* ((syscall (execute-command-with-string-out "ipconfig"))
         (IPv4 (regexp-match #rx"IPv4[^\r]*" syscall)))
    (if IPv4
        (let ((addr (find-ipv4-in-string (car IPv4))))
          (if addr
              (car addr)
              #f))
        #f)))

(define (get-ip-address-linux)
  (let* ((syscall (execute-command-with-string-out "hostname -I"))
         (addr (find-ipv4-in-string syscall)))
    (if addr
        (car addr)
        #f)))

(define (get-ip-address-mac)
  (let* ((syscall (execute-command-with-string-out "ipconfig getifaddr en1"))
         (addr (find-ipv4-in-string syscall)))
    (if addr
        (car addr)
        #f)))
 
(define (get-ip-address)
  (let ((sys (system-type)))
    (cond ((eq? sys 'windows) (get-ip-address-windows))

          ((eq? sys 'unix) (get-ip-address-linux))

          ((eq? sys 'macosx) (get-ip-address-mac))

          (else (error "het gebruikte systeem is niet herkend of wordt niet ondersteund")))))
