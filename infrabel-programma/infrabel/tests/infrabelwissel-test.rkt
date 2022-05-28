#lang racket
(require "../infrabelwissel.rkt"
         rackunit
         rackunit/text-ui
         mock)


(define (simulator-simulator)
  (let ((switch-pos 1))
    (define (dispatch msg . args)
      (apply
       (cond ((eq? msg 'get-switch-position)
              (lambda (id)
                switch-pos))
             ((eq? msg 'set-switch-position!)
              (lambda (id nieuw)
                (set! switch-pos nieuw))))
       args))
    dispatch))

(define de-mock (mock #:behavior (simulator-simulator)))

(define infrabelwissel-test-suite
  (test-suite
   "Infrabelwissel"
   (let ((wissel (maak-infrabelwissel 'test #:switch-object de-mock)))

     (test-case
      "de stand van de wissel opvragen"
      (let ((stand (wissel 'wisselstand)))
        (check-= stand 1 0.0 "stand test")))
     
     (test-case
      "de stand van de wissel aanpassen"
      ((wissel 'pas-wisselstand-aan!) 2)
      (check-= (wissel 'wisselstand) 2 0)
      (check-= (de-mock 'get-switch-position 'test)  (wissel 'wisselstand) 0 "wisselstand aanpassen"))
     
     (test-case
      "wisselstand veranderen"
      ((wissel 'verander-wisselstand!))
      (check-= (de-mock 'get-switch-position 'test)  (wissel 'wisselstand) 0 "wisselstand aanpassen"))
     
     (test-case
      "buren toevoegen"
      ((wissel 'set-buren!) '(1-1 1-2 1-3))
      (check-= ((wissel 'buur?) '1-1) 0 0)
      (check-= ((wissel 'buur?) '1-2) 1 0)
      (check-= ((wissel 'buur?) '1-3) 2 0))

     (test-case
      "wisselstand aanpassen volgens buren"

      ((wissel 'pas-wisselstand-aan-volgens-buur!) '1-1)
      (check-= (wissel 'wisselstand) 1 0)

      ((wissel 'pas-wisselstand-aan-volgens-buur!) '1-2)
      (check-= (wissel 'wisselstand) 1 0)

      ((wissel 'pas-wisselstand-aan-volgens-buur!) '1-3)
      (check-= (wissel 'wisselstand) 2 0)))))
    

(run-tests infrabelwissel-test-suite)