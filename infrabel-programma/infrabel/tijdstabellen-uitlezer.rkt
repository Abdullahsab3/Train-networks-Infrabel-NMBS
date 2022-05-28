#lang racket

(require "../shared-code/constanten.rkt"
         "../shared-code/helpers/timer.rkt")


(provide maak-tijdstabellen-uitlezer)

(define (maak-tijdstabellen-uitlezer vind-trein
                                     vind-detectieblok
                                     vind-wissel
                                     stuur-trein!)
  
  (let ((huidige-tijdstabellen (make-hash)))

    (define (voeg-tijdstabel-toe! trein-id tijdstabel)
      (hash-set! huidige-tijdstabellen trein-id tijdstabel))

    (define (verwijder-tijdstabel! trein-id)
      (hash-remove! huidige-tijdstabellen trein-id))

    ;; zal gegegeven een pas uitgelezen tijdstabel een tijdstbael aanmaken en starten
    (define (maak-tijdstabel ruw-tijdstabel)
      (match ruw-tijdstabel
        [(cons (list (? symbol? trein-id)
                     (? number? trein-snelheid))
               traject)
         
         (let ((trein (vind-trein trein-id)))
           (if trein
               (error "trein met gegeven informatie bestond al")
               (let ((vorig-segment (car traject))
                     (volgend-segment (cadr traject)))
                 (stuur-trein! trein-id trein-snelheid vorig-segment volgend-segment)
                 (define tijdstabel (vector (cons (vind-trein trein-id)
                                                  trein-snelheid)
                                            traject
                                            (make-timer)))
           
                 (voeg-tijdstabel-toe! trein-id tijdstabel)
                 (voer-traject-tussen-db-uit tijdstabel)
                 (list trein-id vorig-segment volgend-segment))))]

        [else (error "formaat van het tijdstabel is niet correct")]))

    (define (maak-tijdstabellen tijdstabellen)
      (let ((nieuwe-treinen '()))
        (if (null? tijdstabellen)
            (error "geen tijdstabellen gegeven")
            (for-each (lambda (tt)
                        (set! nieuwe-treinen  (cons (maak-tijdstabel tt)
                                                    nieuwe-treinen)))
                      tijdstabellen))
        
        nieuwe-treinen))
   

    (define (get-trein-info tijdstabel)
      (vector-ref tijdstabel trein-info-idx))
    
    (define (get-timer tijdstabel)
      (vector-ref tijdstabel timer-idx))
    
    (define (get-traject tijdstabel)
      (vector-ref tijdstabel 1))
    
    (define get-trein car)
    
    (define get-snelheid cdr)

    (define (handle-counting tijdstabel)
      (let* ((trein (get-trein (get-trein-info tijdstabel)))
             (timer (get-timer tijdstabel))
             (tijd-gepasseerd? (timer 'time-passed?)))
        
        (when tijd-gepasseerd?
          ((trein 'pas-snelheid-aan!) (get-snelheid (get-trein-info tijdstabel)))
          (voer-traject-tussen-db-uit tijdstabel))))

    (define (handle-waiting tijdstabel)
      (let* ((trein (get-trein (get-trein-info tijdstabel)))
             (traject (get-traject tijdstabel))
             (first (car traject))
             (doorrijdende-trein-id (cadr first))
             (doorrijdende-trein (vind-trein doorrijdende-trein-id))
             (detectieblok-id (caddr first))
             (mode (cadddr first)))
        
        (when (eq? ((doorrijdende-trein 'update-locatie)) detectieblok-id)
          (cond ((eq? mode 'passed)
                 ((trein 'pas-snelheid-aan!) (get-snelheid (vector-ref tijdstabel trein-info-idx)))
                 (voer-traject-tussen-db-uit tijdstabel))
                         
                ((eq? mode 'passing)
                 (vector-set! tijdstabel 1  (maak-doorrijdend-commando
                                             doorrijdende-trein-id
                                             detectieblok-id
                                             traject)))
                ((eq? mode 'halt)
                 (when (zero? (doorrijdende-trein 'snelheid))
                   ((trein 'pas-snelheid-aan!) (get-snelheid (get-trein-info tijdstabel)))
                   (voer-traject-tussen-db-uit tijdstabel)))))))

    (define (handle-detectieblok-uitvoering tijdstabel)
      (let* ((traject (get-traject tijdstabel))
             (trein (get-trein (get-trein-info tijdstabel)))
             (treinlocatie ((trein 'update-locatie))))
        
        (if (and (vind-detectieblok (car traject))
                 (eq? treinlocatie (car traject)))
            (voer-traject-tussen-db-uit tijdstabel)
            (let ((detbl-in-traject (member treinlocatie traject)))
              (when detbl-in-traject
                (vector-set! tijdstabel 1 detbl-in-traject)
                (voer-traject-tussen-db-uit tijdstabel))))))
        

    (define (counting? cmd) (eq? cmd 'counting))
    (define (waiting? cmd) (and (pair? cmd)
                                (eq? (car cmd) 'wait)))

    (define (volgend-detectieblok-in-traject traject)
      (let loop ((lst traject))
        (cond ((null? lst) #f)
              ((vind-detectieblok (car lst)) lst)
              (else (loop (cdr lst))))))
    
    (define (run-tijdstabel! tijdstabel)
      (let* ((trein (get-trein (get-trein-info tijdstabel)))
             (treinlocatie ((trein 'update-locatie)))
             (traject (get-traject tijdstabel))
             (first (car traject)))
        
        (cond ((counting? first)
               
               (handle-counting tijdstabel))
              
              ((waiting? first)
               
               (handle-waiting tijdstabel))

              (treinlocatie
               (handle-detectieblok-uitvoering tijdstabel)))))
                                 
                                 

    (define (voer-reverse-uit trein commando timer tijdstabel commando-counter)
      ((trein 'reverse-richting!))
      analyseer-de-volgende)

    (define (voer-stop-uit trein commando timer tijdstabel commando-counter . args)
      (cond ((null? args) (verwijder-tijdstabel! (trein 'id)))
            
            ((null? (car args)) ((trein 'pas-snelheid-aan!) 0)
                                (verwijder-tijdstabel! (trein 'id)))
            
            (else (let ((tijd (caar args)))
                    ((trein 'pas-snelheid-aan!) 0)
                    ((timer 'start!) tijd)
                    (vector-set! tijdstabel 1 (cons 'counting
                                                    (cdr commando-counter))))))
      analyseer-niet-verder)

    (define (voer-set-speed-uit trein timer tijdstabel commando-counter args)
      ((trein 'pas-snelheid-aan!) (car args))
      analyseer-de-volgende)

    (define (maak-doorrijdend-commando trein db commando-counter)
      (cons (list 'wait trein db 'passed) (cdr commando-counter)))

    (define (maak-halt-commando trein db commando-counter)
      (cons (list 'wait trein db 'halt) (cdr commando-counter)))

    (define (voer-wait-uit trein timer tijdstabel commando-counter args)
      (let* ((doorrijdende-trein-id (car args))
             (wachtend-detectieblok (cadr args))
             (mode (caddr args))
             (doorrijdende-trein (vind-trein doorrijdende-trein-id)))

        (unless (= (trein 'snelheid) 0)
          ((trein 'pas-snelheid-aan!) 0))
        
        (if doorrijdende-trein
            (if (eq? wachtend-detectieblok ((doorrijdende-trein 'update-locatie)))
                (begin  (vector-set! tijdstabel 1 (cond ((eq? mode 'passing)
                                                         (maak-doorrijdend-commando doorrijdende-trein-id wachtend-detectieblok commando-counter))
                                                        ((eq? mode 'halt)
                                                         (maak-halt-commando doorrijdende-trein-id wachtend-detectieblok commando-counter))
                                                        (else
                                                         (error "ongekende waiting mode"))))
                        analyseer-niet-verder)
                analyseer-niet-verder)
            analyseer-de-volgende)))
   
    (define (voer-stap-uit trein commando timer tijdstabel commando-counter)
      (let ((wissel (vind-wissel commando)))
        (if wissel
            (begin
              ((wissel 'pas-wisselstand-aan-volgens-buur!) (cadr commando-counter))
              analyseer-de-volgende)
            (let ((detectieblok (vind-detectieblok commando)))
              (cond (detectieblok
                     (vector-set! tijdstabel traject-idx commando-counter)
                     analyseer-niet-verder)
                    (else analyseer-de-volgende))))))

    (define (voer-commando-zonder-args-uit!  trein cm timer tijdstabel commando-counter)
      (cond ((eq? cm 'reverse)
             (voer-reverse-uit trein cm timer tijdstabel commando-counter))   
            ((eq? cm 'stop)
             (voer-stop-uit trein cm timer tijdstabel commando-counter))      
            (else
             (voer-stap-uit trein cm timer tijdstabel commando-counter))))

    (define (voer-commando-met-args-uit! trein cm timer tijdstabel commando-counter args)
      (cond ((eq? cm 'stop)
             (voer-stop-uit trein cm timer tijdstabel commando-counter args))
            
            ((eq? cm 'set-speed)
             (voer-set-speed-uit trein timer tijdstabel commando-counter args))
            
            ((eq? cm 'wait)
             (voer-wait-uit trein timer tijdstabel commando-counter args))
            
            (else (error "ongekend commando"))))
    
      

    (define (voer-commando-uit! tijdstabel commando-counter)
      (let* ((trein (get-trein (get-trein-info tijdstabel)))
             (commando (car commando-counter))
             (timer (vector-ref tijdstabel timer-idx)))

        (match commando
          [(? symbol? cm)
           (voer-commando-zonder-args-uit!  trein cm timer tijdstabel commando-counter)]
          
          [(cons (? symbol? cm) args)
           (voer-commando-met-args-uit! trein cm timer tijdstabel commando-counter args)]
           
          [else (error "ongekend commando")])))

    
    (define (voer-traject-tussen-db-uit tijdstabel)
      (let* ((trein (get-trein  (get-trein-info tijdstabel)))
             (traject (vector-ref tijdstabel traject-idx)))
        
        (let loop ((segmenten (cdr traject)))
          (if (null? segmenten)
              (verwijder-tijdstabel! (trein 'id))
              (let ((voer-verder-uit? (voer-commando-uit! tijdstabel segmenten)))
                (when voer-verder-uit?
                  (loop (cdr segmenten))))))))


    (define (run-tijdstabellen!)
      (hash-for-each huidige-tijdstabellen (lambda (key val)
                                             (run-tijdstabel! val))))

    (define (dispatch msg)
      (cond ((eq? msg 'run-tijdstabellen!) run-tijdstabellen!)
            ((eq? msg 'maak-tijdstabellen) maak-tijdstabellen)
            ((eq? msg 'verwijder-tijdstabel!) verwijder-tijdstabel!)
            (else (error "ongekend bericht" msg))))
    
    dispatch))