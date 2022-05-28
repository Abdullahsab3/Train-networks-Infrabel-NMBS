#lang racket


(require "switch-simulator-hardware.rkt"
         "../shared-code/super-adts/superwissel.rkt")


(provide maak-infrabelwissel)

(define (maak-infrabelwissel id #:switch-object [switch-object switch-object])
  (let ((parent (maak-superwissel id)))

    (define (update-wisselstand!)
      (let ((nieuw (switch-object 'get-switch-position id)))
        ((parent 'pas-wisselstand-aan!) nieuw)
        nieuw))

    (define (pas-wisselstand-aan! nieuw)
      (switch-object 'set-switch-position! id nieuw)
      ((parent 'pas-wisselstand-aan!) nieuw))

    (define (pas-wisselstand-aan-volgens-buur! buur-id)
      (let ((buur-idx ((parent 'buur?) buur-id)))

        (if (and buur-idx (> buur-idx 0))
            (pas-wisselstand-aan! buur-idx)
            buur-idx)))

    (define (verander-wisselstand!)
      (if (= (update-wisselstand!) 1)
          (pas-wisselstand-aan! 2)
          (pas-wisselstand-aan! 1)))

    (define (dispatch msg)
      (cond ((eq? msg 'wisselstand) (update-wisselstand!))
            ((eq? msg 'pas-wisselstand-aan!) pas-wisselstand-aan!)
            ((eq? msg 'verander-wisselstand!) verander-wisselstand!)
            ((eq? msg 'pas-wisselstand-aan-volgens-buur!) pas-wisselstand-aan-volgens-buur!)
            (else (parent msg))))
    
    dispatch))