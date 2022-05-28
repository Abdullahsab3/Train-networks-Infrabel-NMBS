#lang racket

(require "../infrabeltrein.rkt"
         rackunit
         rackunit/text-ui
         mock)

(define default-snelheid 100)

(define (simul)
  (let ((snelheid default-snelheid))
    
    (define (obj-simulator msg . args)
      (apply (cond ((eq? msg 'add-loco)
                    (lambda (arg1 arg2 arg3)

                      msg))
          
                   ((eq? msg 'get-loco-speed)
                    (lambda (id)
                      snelheid))
                   ((eq? msg 'get-loco-detection-block)
                    (lambda (id)
                      'testdetectieblok))
                   ((eq? msg 'set-loco-speed!)
                    (lambda (id nieuw)
                      (set! snelheid nieuw)))) args))
    obj-simulator))

(define t (mock #:behavior (simul)))

(define infrabeltrein-test-suite
  (test-suite
   "Infrabeltrein tests"
   (let ((trein (maak-trein 'treinid
                            'eerstesegment-id
                            'tweedesegmentid
                            #:switch-object t)))
     (test-case
      "snelheid van de trein opvragen"
      (let ((snelheid (trein 'snelheid)))
        (check-= default-snelheid snelheid 0.0 "Snelheid opvragen check")))
    
     (test-case
      "snelheid van de trein aanpassen"
      ((trein 'pas-snelheid-aan!) 50)
      (check-= (trein 'snelheid) 50 0 "Snelheid aanpassen check"))
    
     (test-case
      "locatie van de trein opvragen"
      (let ((detbl ((trein 'update-locatie)))
            (detbl-in-simul (t 'get-loco-detection-block 'treinid)))
        (check-eq? detbl detbl-in-simul)))
     (test-case
      "richting van de trein aanpassen"
      ((trein 'rijd-vooruit!))
      (check-pred negative? (t 'get-loco-speed 'treinid) "Trein vooruit rijden")
      ((trein 'rijd-achteruit!))
      (check-pred positive? (t 'get-loco-speed 'treinid) "Trein vooruit rijden"))
     
    
     )))
 

(run-tests infrabeltrein-test-suite)
   
   
