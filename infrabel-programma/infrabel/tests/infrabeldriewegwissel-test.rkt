#lang racket

(require "../infrabeldriewegwissel.rkt"
         rackunit
         rackunit/text-ui
         mock)


(define (wissel-simul)
  (let ((wisselstand 1))
    
    (define (dispatch msg)
      (cond ((eq? msg 'wisselstand) wisselstand)
            ((eq? msg 'pas-wisselstand-aan!) (lambda (x)
                                               (set! wisselstand x)))))
    dispatch))


(define driewegwissel-test-suite
  (test-suite
   "driewegwissel in Infrabel"
   (let* ((eerste-wissel-simul (wissel-simul))
          (tweede-wissel-simul (wissel-simul))
          (driewegwissel (maak-driewegwissel 'test eerste-wissel-simul tweede-wissel-simul)))
     (test-case
      "wisselstand opvragen"
      (let ((wisselstand (driewegwissel 'wisselstand)))
        (check-= wisselstand 1 0 "eerste stand")
        ((eerste-wissel-simul 'pas-wisselstand-aan!) 2)
        (set! wisselstand (driewegwissel 'wisselstand))
        (check-= wisselstand 2 0 "tweede stand")
        ((tweede-wissel-simul 'pas-wisselstand-aan!) 2)
        (set! wisselstand (driewegwissel 'wisselstand))
        (check-= wisselstand 3 0 "derde stand")))
     
     (test-case
      "wisselstand aanpassen"
      ((driewegwissel 'pas-wisselstand-aan!) 1)
      (check-= (eerste-wissel-simul 'wisselstand) 1 0)
      ((driewegwissel 'pas-wisselstand-aan!) 2)
      (check-= (eerste-wissel-simul 'wisselstand) 2 0)
      (check-= (tweede-wissel-simul 'wisselstand) 1 0)
      ((driewegwissel 'pas-wisselstand-aan!) 3)
      (check-= (eerste-wissel-simul 'wisselstand) 2 0)
      (check-= (tweede-wissel-simul 'wisselstand) 2 0)))))

(run-tests driewegwissel-test-suite)