#lang racket

(require "../helpers/objs-helpers-functies.rkt")
(require (prefix-in error-handler: "../helpers/error-handling.rkt"))
(require "../constanten.rkt")

(provide maak-superspoormodel)

(define (maak-superspoormodel)
  
  (define (maak-detectieblokken ids aanmaakoperatie)
    (maak-objecten-uit-ids ids aanmaakoperatie))
  
  (define (maak-wissels ids aanmaakoperatie)
    (maak-objecten-uit-ids ids aanmaakoperatie))
  
  (let ((detectieblokken (make-hash))
        (wissels (make-hash))
        (segmenten (make-hash)))

    (define (voeg-segment-toe! id seg)
      (hash-set! segmenten id seg))

    (define (vind-segment id)
      (hash-ref segmenten id #f))

    (define (voeg-detectieblok-toe! detbl-id detbl)
      (hash-set! detectieblokken detbl-id detbl))

    (define (voeg-wissel-toe! wissel-id wissel)
      (hash-set! wissels wissel-id wissel))

    (define (get-detectieblokken-ids)
      (get-objs-ids detectieblokken))
    
    (define (get-wissels-ids)
      (get-objs-ids wissels))
    
    (define (vind-detectieblok detbl-id)
      (hash-ref detectieblokken detbl-id #f))
    
    (define (vind-wissel wissel-id)
      (hash-ref wissels wissel-id #f))

    (define (get-detectieblok-buren detbl-id)
      (let ((detectieblok (vind-detectieblok detbl-id)))
        ((detectieblok 'get-naburige-segmenten-ids))))

    (define (is-begseg-van-detectieblok? detblid id)
      (let* ((detectieblok (vind-detectieblok detblid))
             (begseg (detectieblok 'begseg)))
        (eq? begseg id)))

    (define (set-buren! seg ids)
      (unless (null? ids)
        ((seg 'set-buren!) ids)))

    
    ; (DB (bg) (einde))
    ; (DB () einde)
    ; (DB (bg) ())
    ; (SW (bg) (einde einde))
    ; (SW (bg) (einde einde einde))
    (define (to-buren-list seg lst)
      (if (null? (cdr lst))
          (error-handler:raise-error wrong-setupformat seg)
          (match lst

            [(list (? null? geen-beginbuur) (? null? geen-eindbuur))
             (error-handler:raise-error wrong-connections seg)]
        
            [(list (? null? geen-beginbuur) (? list? eindsegmenten))
             (cons #f eindsegmenten)]
        
            [(list (list beginsegment) (? null? geen-eindbuur))
             (list beginsegment #f)]
        
            [(list (list beginsegment) (? list? eindsegmenten))
             (cons beginsegment eindsegmenten)]
        
            [else (error-handler:raise-error wrong-setupformat seg)])))
   
    
    (define (set-segment-buren! seg beg&end)
      (let ((segment (vind-segment seg)))
        (if segment
            (let ((buren (to-buren-list seg beg&end)))
              (if buren
                  (set-buren! segment buren)
                  failed))
            (error-handler:raise-error nonexisting-segment seg))))
    


    (define (setup-segmenten lsts)
      (let loop ((curr lsts))
        (unless (null? curr)
          (match (car curr)
            [(cons (? symbol? seg-id) conns)
             (if (set-segment-buren! seg-id conns)
                 (loop (cdr curr))
                 failed)]
            [else (error-handler:raise-error wrong-setupformat)]))))

    (define (setup-segmenten-bij-initialisatie lsts maak-blok)
      (let loop ((curr lsts))
        (unless (null? curr)
          (match (car curr)
            [(cons (? symbol? seg-id) conns)
             (let ((detbl (vind-detectieblok seg-id)))
               (if detbl
                   (voeg-segment-toe! seg-id detbl)
                   (let ((wissel (vind-wissel seg-id)))
                     (if wissel
                         (voeg-segment-toe! seg-id wissel)
                         (voeg-segment-toe! seg-id (maak-blok seg-id))))))
             (if (set-segment-buren! seg-id conns)
                 (loop (cdr curr))
                 failed)]

            [else (error-handler:raise-error wrong-setupformat)]))))
                   

    (define (pas-wissel-aan! wissel-id nieuw)
      (hash-set! wissels wissel-id nieuw))

    (define (update-detectieblokken! laatste-detbl-id huidige-detbl-id trein-id)
      (let ((laatste-detectieblok (vind-detectieblok laatste-detbl-id))
            (huidige-detectieblok (vind-detectieblok huidige-detbl-id)))
        (when laatste-detectieblok
          ((laatste-detectieblok 'set-aanwezige-trein!) #f))
        (when huidige-detectieblok
          ((huidige-detectieblok 'set-aanwezige-trein!) trein-id))))
        
    (define (initialiseer detectieblokken-ids
                          detectieblok-aanmaak-operatie
                          wissels-ids
                          wissel-aanmaakoperatie
                          blok-aanmaakoperatie
                          connecties)
      
      (set! detectieblokken (maak-detectieblokken detectieblokken-ids detectieblok-aanmaak-operatie))
      (set! wissels (maak-wissels wissels-ids wissel-aanmaakoperatie))
      (setup-segmenten-bij-initialisatie connecties blok-aanmaakoperatie))



    (define (dispatch msg)
      
      (cond ((eq? msg 'get-detectieblokken-ids) get-detectieblokken-ids)
            ((eq? msg 'get-detectieblok-buren) get-detectieblok-buren)
            ((eq? msg 'vind-detectieblok) vind-detectieblok)
            ((eq? msg 'update-detectieblokken!) update-detectieblokken!)
            ((eq? msg 'is-begseg-van-detectieblok?) is-begseg-van-detectieblok?)
            

            ((eq? msg 'setup-segmenten) setup-segmenten)
            ((eq? msg 'vind-segment) vind-segment)

            
            ((eq? msg 'get-wissels-ids) get-wissels-ids)
            ((eq? msg 'vind-wissel) vind-wissel)
            ((eq? msg 'pas-wissel-aan!) pas-wissel-aan!)

            ((eq? msg 'get-ruwe-wissels-ids) (hash-keys wissels))
            ((eq? msg 'get-ruwe-detectieblokken-ids) (hash-keys detectieblokken))
            ((eq? msg 'get-ruwe-segmenten-ids) (hash-keys segmenten))

            ((eq? msg 'voeg-detectieblok-toe!) voeg-detectieblok-toe!)
            ((eq? msg 'voeg-wissel-toe!) voeg-wissel-toe!)
            ((eq? msg 'voeg-segment-toe!) voeg-segment-toe!)

            
            ((eq? msg 'initialiseer) initialiseer)
            (else (error "ongekende boodschap" msg))))
    
    dispatch))
