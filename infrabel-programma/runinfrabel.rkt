#lang racket

(require "infrabel/infrabel.rkt")

(define hardware? #f)
(define infrabel #f)

(define (print-input)
  (displayln "Welkom in Infrabel!")
  (displayln "Maak een keuze:")
  (displayln "1-  de fysieke hardware opstelling")
  (displayln "2-  de simulator versie")
  (displayln "geef het getal van de keuze in"))

(define (get-user-input)
  (let ((in (read)))
    (cond ((member in '(1 HW H h hardware)) (set! hardware? #t))
          ((member in '(2 s S simulator)) (set! hardware? #f))
          ((member in '(exit verlaat e)) (exit))
          (else (displayln  "Ongekende commando. Gelieve opnieuw te proberen")
                (get-user-input)))))

(define (start-infrabel)
  (print-input)
  (get-user-input)
  (set! infrabel (maak-infrabel hardware?))
  ((infrabel 'activeer!))
  (let loop ()
    (unless ((infrabel 'geactiveerd?))
      (sleep 1)
      (loop))))
  

(start-infrabel)
