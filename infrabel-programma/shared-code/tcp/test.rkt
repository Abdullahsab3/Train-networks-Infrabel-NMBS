#lang racket

 (define-values (in out) (tcp-connect ip receiver-port))
    (write info out)
    (flush-output out)