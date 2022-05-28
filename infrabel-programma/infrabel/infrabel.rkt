#lang racket

(require "switch-simulator-hardware.rkt"
         "infrabeltrein.rkt"
         (prefix-in error-handler: "../shared-code/helpers/error-handling.rkt")
         "../shared-code/constanten.rkt"
         "tijdstabellen-uitlezer.rkt"
         "hardware-spoormodel.rkt"
         "extern-infrabelspoormodel.rkt"
         "../shared-code/tcp/receiver.rkt"
         "../shared-code/tcp/ip-address.rkt"
         "../shared-code/constanten.rkt")


(provide maak-infrabel)

(define (maak-infrabel hardware?
                       #:switch-object [switch-object (set-switch-object! hardware?)]
                       #:spoormodel [maak-hardware-spoormodel maak-hardware-spoormodel]
                       #:extern-spoormodel [maak-extern-infrabelspoormodel maak-extern-infrabelspoormodel]
                       #:trein [maak-trein maak-trein]
                       #:tijdstabellen-uitlezer [maak-tijdstabellen-uitlezer maak-tijdstabellen-uitlezer])
  
 
  ;; treinen worden opgeslagen in een vector-object
  (define maak-treinen-opslag make-hash)
  ;; een variabele om de thread bij te houden.
  (define th #f)
  (define th2 #f)


  (let ((treinen (maak-treinen-opslag))
        (spoormodel #f)
        (hardware-opstelling? #f)
        (tijdstabellen #f)
        (receiver #f))


    (define (initialiseer-infrabel!)
      (switch-object 'start))
      

    (define (spooropstelling-parts-exist? defs segs conns)
      
      (cond ((null? defs)
             (error-handler:raise-error empty-type-definition))

            ((null? segs)
             (error-handler:raise-error empty-segments-definition))

            ((null? conns)
             (error-handler:raise-error empty-connections))
            
            (else #t)))
    
    (define (laad-opstelling-in definities ruwe-segmenten ruwe-connecties)
      (if (spooropstelling-parts-exist? definities ruwe-segmenten ruwe-connecties)
          (let ((execution-result-from-simulator
                 (switch-object 'laad-opstelling-in definities
                                ruwe-segmenten
                                ruwe-connecties)))
            (cond (execution-result-from-simulator
                   (let ((opstelling (maak-extern-infrabelspoormodel definities ruwe-segmenten ruwe-connecties)))
                     (cond (opstelling
                            (set! spoormodel opstelling)
                            (initialiseer-infrabel!)
                            executed)
                           (else (error-handler:error-to-CLI)))))
                  (else (error-handler:error-to-CLI))))
          (error-handler:error-to-CLI)))

    (define (start-hardware-opstelling!)
      (switch-object 'setup-hardware)
      (set! hardware-opstelling? #t)
      (set! spoormodel (maak-hardware-spoormodel))
      ((spoormodel 'initialiseer))
      (initialiseer-infrabel!))

    (define (laad-tijdstabellen-in! ruwe-tijdstabellen)
      ((tijdstabellen 'maak-tijdstabellen) ruwe-tijdstabellen))

    (define (run-tijdstabellen!)
      ((tijdstabellen 'run-tijdstabellen!)))

    (define (activeer!)
      (displayln (~a "Het ip adres van Infrabel is: " (get-ip-address)))
      (set! receiver (maak-receiver infraport dispatch))
      (set! th (thread luister-loop))
      ;; infrabel activeren zal ook de loop van infrabel starten.
      (set! th2 (thread update-loop)))

    (define (deactiveer!)
      (when switch-object
        (switch-object 'stop))
      (when receiver
        ((receiver 'sluit-connectie)))
      (when (thread? th)
        (kill-thread th))
      (when (thread? th2)
        (kill-thread th2)))

    (define (voeg-trein-toe! trein-id trein)
      (hash-set! treinen trein-id trein))

    ;; dit wordt gebruikt om te zorgen dat alle treinen vooruit beginnen rijden.
    (define (is-begseg-van-detectieblok? detblid id)
      ((spoormodel 'is-begseg-van-detectieblok?) detblid id))

    (define (kalibreer-nieuwe-trein trein snelheid vorig-segment beginsegment)
      
      ;; richting: vooruit => tegenwijzezin, achteruit => wijzezin
      ;; richting wordt bij aanmaak van de trein bepaald op basis van de segmenten waaruit de trein vertrekt.
      (if (is-begseg-van-detectieblok? vorig-segment beginsegment)
          ((trein 'rijd-achteruit!))
          (begin ((trein 'pas-rijrichting-aan!))
                 ((trein 'rijd-vooruit!))))
      ((trein 'pas-snelheid-aan!) snelheid))


    (define (stuur-trein! trein-id snelheid vorig-segment beginsegment)
      (let* ((trein (maak-trein trein-id vorig-segment beginsegment)))
        (voeg-trein-toe! trein-id trein)
        (kalibreer-nieuwe-trein trein snelheid vorig-segment beginsegment)))

    (define (verwijder-trein! trein-id)
      ((tijdstabellen 'verwijder-tijdstabel!) trein-id)
      (hash-remove! treinen trein-id)
      (switch-object 'remove-loco trein-id))

    (define (vind-trein id)
      (hash-ref treinen id #f))
    
    (define (trein-snelheid treinid)
      (let ((trein (vind-trein treinid)))
        (when trein
          (trein 'snelheid))))

    (define  (trein-vooruit? id)
      (let ((trein (vind-trein id)))
        (trein 'vooruit?)))

    (define (pas-trein-snelheid-aan! treinid snelheid)
      (let ((trein (vind-trein treinid)))
        ((trein 'pas-snelheid-aan!) snelheid)))

    (define (rijd-vooruit! treinid)
      (let ((trein (vind-trein treinid)))
        ((trein 'rijd-vooruit!))))
    
    (define (rijd-achteruit! treinid)
      (let ((trein (vind-trein treinid)))
        ((trein 'rijd-achteruit!))))

    (define (vind-detectieblok id)
      ((spoormodel 'vind-detectieblok) id))

    ;; deze procedures zorgen ervoor dat de detectieblokken weten welke trein momenteel op hen aanwezig is.
    (define (update-detectieblokken! laatste-detbl-id huidige-detbl-id trein-id)
      ((spoormodel 'update-detectieblokken!) laatste-detbl-id huidige-detbl-id trein-id))

    (define (update-treinlocatie trein)
      (let ((laatste-locatie (trein 'locatie))
            (huidige-locatie ((trein 'update-locatie))))

        (unless (or (eq? huidige-locatie laatste-locatie)
                    (false? huidige-locatie))
          (update-detectieblokken! laatste-locatie huidige-locatie (trein 'id)))
        (if (false? huidige-locatie)
            laatste-locatie
            huidige-locatie)))

    (define (vind-trein-update-treinlocatie trein-id)
      (let ((trein (vind-trein trein-id)))
        (when trein
          (update-treinlocatie trein))))

    (define (vind-wissel id)
      ((spoormodel 'vind-wissel) id))

    (define (wisselstand wisselid)
      (let ((wissel (vind-wissel wisselid)))
        (wissel 'wisselstand)))

    (define (pas-wisselstand-aan! wisselid stand)
      (let ((wissel (vind-wissel wisselid)))
        ((wissel 'pas-wisselstand-aan!) stand)))

    (define (verander-wisselstand! wisselid)
      (let ((wissel (vind-wissel wisselid)))
        ((wissel 'verander-wisselstand!))))

    (define (voor-elke-trein proc)
      (hash-for-each treinen 
                     (lambda (trein-id trein)
                       (proc trein))))

    (define (get-treinen-ids-en-snelheden)
      (hash-map treinen
                (lambda (trein-id trein)
                  (cons trein-id (trein 'snelheid)))))
   
    (define (luister-loop)
      ((receiver 'check-inkomende-berichten!))
      (luister-loop))
    
    (define (update-loop)
      (when spoormodel
        (run-tijdstabellen!)
        (voor-elke-trein
         (lambda (trein)
           (update-treinlocatie trein)
           (trein 'snelheid))))
      (update-loop))

    (define (trein-aanwezig? detbl-id)
      (let ((detectieblok (vind-detectieblok detbl-id)))
        (detectieblok 'trein-aanwezig?)))
      
    (define (get-detectieblok-buren detblid)
      ((spoormodel 'get-detectieblok-buren) detblid))

    
    (define (verbind-met-NMBS)
      (if spoormodel
          (cons hardware-opstelling?
                (if hardware-opstelling?
                    '()
                    ((spoormodel 'get-opstelling-informatie))))
          #t))

    (define (geactiveerd?)
      (and th2 spoormodel))

    (define (get-detectieblokken-ids)
      (spoormodel 'get-ruwe-detectieblokken-ids))

    (define (get-wissels-ids)
      (spoormodel 'get-ruwe-wissels-ids))

    (define (dispatch msg)
      (cond
        ((eq? msg 'activeer!) activeer!)
        ((eq? msg 'deactiveer!) deactiveer!)
        ((eq? msg 'geactiveerd?) geactiveerd?)
        ((eq? msg 'stuur-trein!) stuur-trein!)
        ((eq? msg 'verwijder-trein!) verwijder-trein!)

        ((eq? msg 'laad-opstelling-in) laad-opstelling-in)
        ((eq? msg 'start-hardware-opstelling!) start-hardware-opstelling!)
        ((eq? msg 'laad-tijdstabellen-in!) laad-tijdstabellen-in!)
           
        ((eq? msg 'trein-aanwezig?) trein-aanwezig?)
        ((eq? msg 'pas-trein-snelheid-aan!) pas-trein-snelheid-aan!)
        ((eq? msg 'rijd-vooruit!) rijd-vooruit!)
        ((eq? msg 'rijd-achteruit!) rijd-achteruit!)
        ((eq? msg 'update-treinlocatie) vind-trein-update-treinlocatie)
        ((eq? msg 'trein-snelheid) trein-snelheid)
        ((eq? msg 'trein-vooruit?) trein-vooruit?)
   
        ((eq? msg 'get-detectieblok-buren) get-detectieblok-buren)
            
        ((eq? msg 'wisselstand) wisselstand)
        ((eq? msg 'pas-wisselstand-aan!) pas-wisselstand-aan!)
        ((eq? msg 'verander-wisselstand!) verander-wisselstand!)

        ((eq? msg 'get-detectieblokken-ids) get-detectieblokken-ids)
        ((eq? msg 'get-wissels-ids) get-wissels-ids)

        ((eq? msg 'verbind-met-NMBS) verbind-met-NMBS)
        ((eq? msg 'get-treinen-ids-en-snelheden) get-treinen-ids-en-snelheden)

        (else (error "ongekende boodschap" msg))))
    

    (set! tijdstabellen (maak-tijdstabellen-uitlezer vind-trein
                                                     vind-detectieblok
                                                     vind-wissel
                                                     stuur-trein!))
   
    
    dispatch))
