#lang racket
(require "../tijdstabellen-uitlezer.rkt"
         "../infrabelwissel.rkt"
         "../../shared-code/super-adts/superdetectieblok.rkt"
         rackunit/text-ui
         rackunit
         mock)


(define rijrichting-aangepast? #f)
(define snelheid-aangepast? #f)
(define vooruit? #f)
(define locatie 'test)
(define locatie-geupdatet? #f)
(define (trein-simulator trein-id vorig-segment beginsegment)

  (define (dispatch msg)
    (cond ((eq? msg 'pas-rijrichting-aan!) (lambda ()
                                             (set! rijrichting-aangepast? #t)))
          ((eq? msg 'id) trein-id)
          ((eq? msg 'rijd-vooruit!) (lambda ()
                                      (set! vooruit? #t)))
          ((eq? msg 'pas-snelheid-aan!) (lambda (nieuw)
                                          (set! snelheid-aangepast? nieuw)))
          ((eq? msg 'rijrichting-aangepast?) rijrichting-aangepast?)
          ((eq? msg 'snelheid-aangepast?) snelheid-aangepast?)
          ((eq? msg 'vooruit?) vooruit?)
          ((eq? msg 'locatie) locatie)
          ((eq? msg 'update-locatie) (lambda ()
                                       (set! locatie-geupdatet? #t)
                                       'test))))
    
  dispatch)


(define (simulator-simulator)
  (let ((switch-pos 1))
    (define (dispatch msg)
      (cond ((eq? msg 'get-switch-position)
             (lambda (id)
               switch-pos))
            ((eq? msg 'set-switch-position!)
             (lambda (id nieuw)
               (set! switch-pos nieuw)))))
    dispatch))

(define de-mock (mock #:behavior (simulator-simulator)))

  


(define treinen (list (list 'T-1 (trein-simulator 'T-1 '1-1 '1-2))
                      (list 'T-3 (trein-simulator 'T-3 '1-1 '1-2))))

(define (vind-trein id)
  (let ((gevonden (assoc id treinen)))
    (if gevonden (cadr gevonden) gevonden)))

(define (vind-wissel id)
  (let ((gevonden (assoc id  (list (list 'S-1
                                         (let ((wissel (maak-infrabelwissel 'S-1  #:switch-object de-mock)))
                                           ((wissel 'set-buren!) '(1-1 1-2 1-3))
                                           wissel))))))
    (if gevonden (cadr gevonden) gevonden)))

(define (vind-detectieblok id)
  (let ((gevonden (assoc id (list (list '1-1
                                        (let ((db (maak-superdetectieblok '1-1)))
                                          ((db 'set-buren!) '(S-1 2-3))
                                          db))
                                  (list '1-2
                                        (let ((db (maak-superdetectieblok '1-2)))
                                          ((db 'set-buren!) '(S-1 4-5))
                                          db))
                                  (list '1-3
                                        (let ((db (maak-superdetectieblok '1-3)))
                                          ((db 'set-buren!) '(S-1 5-6))
                                          db))))))
    (if gevonden (cadr gevonden) gevonden)))

(define (stuur-trein! id snelheid vorig-seg beginseg)
  (set! treinen (cons (list id (trein-simulator 'T-3 '1-1 '1-2)) treinen)))

(define tijdstabellen-uitlezer-suite
  (test-suite
   "tijdstabellen-uitlezer test suite"
   (let ((tijdstabel (maak-tijdstabellen-uitlezer  vind-trein
                                                   vind-detectieblok
                                                   vind-wissel
                                                   stuur-trein!)))
     (test-case
      "een tijdstabel toeveogen"
     
      (let ((lst ((tijdstabel 'maak-tijdstabellen) (list '((T-2 250) 1-1 1-2)))))
        (check-true (pair? lst))
        (check-equal? (car lst) '(T-2 1-1 1-2) 0)))

     (test-case
      "een tijdstabel verwijderen"
      ((tijdstabel 'verwijder-tijdstabel!) 'T-2)))))

(run-tests tijdstabellen-uitlezer-suite)