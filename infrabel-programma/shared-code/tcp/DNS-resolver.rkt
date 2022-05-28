#lang racket

(provide resolve-DNS)

(define (execute-powershell-command command)
  (let* ((cmd (process (format  "powershell -command ~s" command)))
         (in (car cmd))
         (out (cadr cmd))
         (err (cadddr cmd)))
    (define info '())
    (let loop ((inkomend (read in)))
      (unless (eof-object? inkomend)
        (set! info (cons inkomend info))
        (loop (read in))))
                   
    (close-output-port out)
    (close-input-port in)
    (close-input-port err)
    
    (if (null? info) #f info)))

(define (resolve-DNS-windows dns)
  (let ((cmd (execute-powershell-command
              (format "Resolve-DnsName -Name ~s -Type A | Select-Object -Property IPAddress | Format-Table -HideTableHeaders" dns))))
    (if cmd
        (symbol->string (car cmd))
        cmd)))

(define (resolve-DNS-unix dns)
  (let* ((cmd (process (format "host -4 ~s | grep -E -o ~s" dns "([0-9]{1,3}[\\.]){3}[0-9]{1,3}")))
         (in (car cmd))
         (out (cadr cmd))
         (err (cadddr cmd)))

    (define info (read in))
    
    (close-output-port out)
    (close-input-port in)
    (close-input-port err)
    
    (if (eof-object? info)
        #f
        info)))
      


(define (resolve-DNS dns)
  (let ((systype (system-type)))
    (if (eq? systype 'windows)
        (resolve-DNS-windows dns)
        (resolve-DNS-unix dns))))