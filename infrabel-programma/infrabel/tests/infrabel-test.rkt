#lang racket

(require "../infrabel.rkt"
         rackunit/text-ui
         rackunit)
(require mock)

(define hardware-setup? #f)
(define gestart? #f)
(define loco-verwijderd? #f)
(define detblids-gevraagd? #f)
(define switchids-gevraagd? #f)
(define opstelling-ingeladen? #f)
(define opstelling-in-simul-ingeladen? #f)

(define (simulator-simulator)
    
  (define (dispatch msg . arg)
    (apply (cond ((eq? msg 'setup-hardware) (lambda () (set! hardware-setup? #t)))
                 ((eq? msg 'laad-opstelling-in) (lambda (a b c) (set! opstelling-in-simul-ingeladen? #t)))
                 ((eq? msg 'start) (lambda () (set! gestart? #t)))
                 ((eq? msg 'stop) (lambda ()
                                    (set! gestart? #f)))
                 ((eq? msg 'remove-loco) (lambda (id)
                                           (set! loco-verwijderd? #t)))
                 ((eq? msg 'get-detection-block-ids) (lambda () (set! detblids-gevraagd? #t)))
                 ((eq? msg 'get-switch-ids) (lambda () (set! switchids-gevraagd? #t))))
           arg))
  dispatch)

(define (spoormodel-simulator)
  (let ((geinit? #t)
        (detectieblokken-geupdatet? #f))


    (define (dispatch msg)
      (cond ((eq? msg 'initialiseer) (lambda () (set! geinit? #t)))
            ((eq? msg 'is-begseg-van-detectieblok?) (lambda (een twee) #f))
            ((eq? msg 'vind-detectieblok) (lambda (el)
                                            (lambda (msg )'iets)))
            ((eq? msg 'geinit?) geinit?)
            ((eq? msg 'update-detectieblokken!) (lambda (een twee)
                                                  (set! detectieblokken-geupdatet? #f)))))
    
    dispatch))

(define (extern-spoormodel-simulator definities ruwe-segmenten ruwe-connecties)
  (if (and (null? definities)
           (null? ruwe-segmenten)
           (null? ruwe-connecties))
      #f
      (begin (set! opstelling-ingeladen? #t)
             (spoormodel-simulator))))
  

(define rijrichting-aangepast? #f)
(define snelheid-aangepast? #f)
(define vooruit? #f)
(define locatie 'test)
(define locatie-geupdatet? #f)
(define (trein-simulator trein-id vorig-segment beginsegment)

  (define (dispatch msg)
    (cond ((eq? msg 'pas-rijrichting-aan!) (lambda ()
                                             (set! rijrichting-aangepast? #t)))
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

(define treinidtijdstabel #f)
(define gemaakt? #f)
(define tijdstabel-verwijderd? #f)
(define (tijdstabellen-uitlezer vind-trein vind-detectieblok vind-wissel stuur-trein!)
    
  (define (dispatch msg)
    (cond ((eq? msg 'run-tijdstabellen!) (lambda () #t))
          ((eq? msg 'maak-tijdstabellen) (lambda (ig) (set! gemaakt? #t)
                                           (set! treinidtijdstabel (caar ig))))
          ((eq? msg 'verwijder-tijdstabel!) (lambda (ig) (when (eq? ig treinidtijdstabel)
                                                           (set! tijdstabel-verwijderd? #t))))))
  
  dispatch)
  

(define simul-mock (mock #:behavior (simulator-simulator)))
(define spoormodel-mock (mock #:behavior spoormodel-simulator))
(define extern-spoomodel-mock (mock #:behavior extern-spoormodel-simulator))
(define trein-mock (mock #:behavior trein-simulator))
(define tijdstabellen-uitlezer-mock (mock #:behavior tijdstabellen-uitlezer))


(define infrabel-test-suite
  (test-suite
   "Infrabel test suite"
   (let ((infrabel (maak-infrabel #f
                                  #:switch-object simul-mock
                                  #:spoormodel spoormodel-mock
                                  #:extern-spoormodel extern-spoomodel-mock
                                  #:trein trein-mock
                                  #:tijdstabellen-uitlezer tijdstabellen-uitlezer-mock)))
   

     (test-case
      "Infrabel starten"
      ((infrabel 'activeer!))
      ((infrabel 'start-hardware-opstelling!))
      (check-true gestart?)
      (check-true ((spoormodel-mock) 'geinit?)))
     
     (test-case
      "infrabel starten met externe opstelling"
      (set! infrabel (maak-infrabel #f
                                    #:switch-object simul-mock
                                    #:spoormodel spoormodel-mock
                                    #:extern-spoormodel extern-spoomodel-mock
                                    #:trein trein-mock
                                    #:tijdstabellen-uitlezer tijdstabellen-uitlezer-mock))
      
      ((infrabel 'laad-opstelling-in) '(D T S)
                                      
                                      '((D 1 G231 G231)
                                        (D 2 G231 G231)
                                        (T 1 G231 G231)
                                        (D 3 G231 G231)
                                        (D 4 G231 G231))
                                      
                                      '((D-1 () (D-2))
                                        (D-2 (D-1) (T-1))
                                        (D-3 (T-1) (D-4))
                                        (D-4 (D-3) ())
                                        (T-1 (D-2) (D-3))))
      
      (check-true opstelling-in-simul-ingeladen?)
      (check-true opstelling-ingeladen?))
     
     (test-case
      "Tijdstabel uitlezen en uitvoeren"
      ((infrabel 'laad-tijdstabellen-in!) '((T-1 50) D-1 D-2))
      (check-true gemaakt?)
      ((infrabel 'verwijder-trein!) 'T-1)
      (check-true tijdstabel-verwijderd?))

     (test-case
      "Trein toevoegen"
      (let* ((snelheid 100))
        
        ((infrabel 'stuur-trein!) 'test snelheid 'det1 'det2)
        
        (check-true vooruit?)
        (check-true rijrichting-aangepast?)
        (check-= snelheid-aangepast? snelheid 0)))

     (test-case
      "Trein verwijderen"
      ((infrabel 'verwijder-trein!) 'whaetever)
      (check-true loco-verwijderd?)))))

(run-tests infrabel-test-suite)
