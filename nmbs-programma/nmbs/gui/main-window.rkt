#lang racket/gui
(require "../../shared-code/constanten.rkt"
         "trein-toevoeging-window.rkt"
         "error-pop-up-window.rkt"
         "../../shared-code/helpers/timer.rkt")

(provide maak-main-gui)

(define (maak-main-gui get-ids
                       voeg-trein-toe
                       verwijder-trein
                       trein-informatie-en-callbacks
                       detectieblokken-informatie
                       wissels-informatie-en-callbacks
                       laad-traject-in)
    

  ;------------------------------------------------
  ;; de verkregen informatie over een trein is bijgehouden in een lijst.
  ;; abstracties om in de lijst te caren/cdren
  (define get-trein-info car)
  (define get-snelheid car)
  (define get-richting cadr)
  (define get-locatie caddr)

  (define get-trein-callbacks cdr)
  (define get-pas-treinsnelheid-aan! car)
  (define get-rijd-vooruit! cadr)
  (define get-rijd-achteruit! caddr)

  
  (define (get-treinsnelheid treinidx)
    (let ((trein-info (get-trein-info (trein-informatie-en-callbacks treinidx))))
      (get-snelheid trein-info)))

  (define (get-treinlocatie treinidx)
    (let ((trein-info (get-trein-info (trein-informatie-en-callbacks treinidx))))
      (get-locatie trein-info)))

  (define (get-treinrichting treinidx)
    (let ((trein-info (get-trein-info (trein-informatie-en-callbacks treinidx))))
      (get-richting trein-info)))

  (define (pas-treinsnelheid-aan! treinidx snelheid)
    (let* ((trein-callbacks (get-trein-callbacks (trein-informatie-en-callbacks treinidx)))
           (pas-snelheid-aan! (get-pas-treinsnelheid-aan! trein-callbacks)))
      (pas-snelheid-aan! snelheid)))

  (define (rijd-vooruit! treinidx)
    (let* ((trein-callbacks (get-trein-callbacks (trein-informatie-en-callbacks treinidx)))
           (rijd-vooruit! (get-rijd-vooruit! trein-callbacks)))
      (rijd-vooruit!)))

  (define (rijd-achteruit! treinidx)
    (let* ((trein-callbacks (get-trein-callbacks (trein-informatie-en-callbacks treinidx)))
           (rijd-achteruit! (get-rijd-achteruit! trein-callbacks)))
      (rijd-achteruit!)))
  
  ;-------------------------
  (define (get-trein-ids)
    (sort (car (get-ids)) string-ci<=?))

  (define (get-detectieblokken-ids)
    (sort (cadr (get-ids)) string-ci<=?))

  (define (get-wissels-ids)
    (sort (caddr (get-ids)) string-ci<=?))
  ;-----------------------------------
  (define detectieblok-buren car)
  (define detectieblok-pred cadr)

  (define (get-detectieblok-informatie detblidx)
    (detectieblokken-informatie detblidx))

  (define (get-detectieblok-buren idx)
    (detectieblok-buren (get-detectieblok-informatie idx)))

  (define (trein-aanwezig? idx)
    ((detectieblok-pred (get-detectieblok-informatie idx))))
  ;-----------------------------------
  (define wisselstand car)
  (define get-pas-wisselstand-aan! cadr)
  (define (get-wisselstand wisselidx)
    (wisselstand (wissels-informatie-en-callbacks wisselidx)))

  (define (verander-wisselstand! wisselidx)
    ((get-pas-wisselstand-aan! (wissels-informatie-en-callbacks wisselidx))))
  ;------------------------------------
  ;; een lijst van children toevoegen aan de parent
  (define (add-children parent children)
    (unless (null? children)
      (let ((child (car children)))
        (unless (send child is-shown?)
          (send parent add-child (car children)))
        (add-children parent (cdr children)))))
  
  (let* ((frame (new frame%
                     [label "GUI"]
                     [width venster-breedte]
                     [height venster-hoogte])))

    ;; dankzij: https://gist.github.com/samdphillips/701e20ab306e8a0c0644437102735b9d


    ;; de inhoud van de gekozen tab weergeven
    (define (vul-tab-panel tab-panel)
      (let ((tab-naam   (send tab-panel get-item-label (send tab-panel get-selection))))
        (send tab-panel change-children
              (lambda (c*)
                (let ((tab (kies-tab tab-naam)))
                  (when (eq? tab-naam treinen-tab)
                    (send treinen-keuzes clear)
                    (when (eq? (system-type) 'unix)
                      (send treinen-keuzes append "Kies een trein")) ;; domme linux-specific bug oplossen :')
                    (for-each (lambda (trein-id)
                                (send treinen-keuzes append trein-id))
                              (get-trein-ids)))
                  tab)))))

    (define (verander-tab tp event)
      (when (eq? (send event get-event-type) 'tab-panel)
        (vul-tab-panel tp)))

    (define (update-label msg label)
      (send msg set-label label))

    (define (get-geselecteerde-id keuzes)
      (let ((answ (send keuzes get-string-selection)))
        (if answ
            (string->symbol (send keuzes get-string-selection))
            #f)))

    
    (define panels (new tab-panel%
                        (parent frame)
                        (choices (list treinen-tab
                                       wissels-tab
                                       detectieblokken-tab
                                       tijdstabellen-tab))
                        (callback verander-tab)))

    ;; een variabele die later de thread bijhoudt
    (define th #f)
    (define thread-timer (make-timer))

    (define (start-timer!)
      ((thread-timer 'start!) 1))

    (define (tijd-gepasseerd?)
      (thread-timer 'time-passed?))
    ;------------ treinen --------------;
    (define (teken-trein-informatie treinidx)
      (update-snelheid! treinidx)
      (update-locatie! treinidx))

    (define (update-snelheid! treinidx)
      (let* ((snelheid (get-treinsnelheid treinidx))
             (label (apply ~a (list "Snelheid: "
                                    (abs snelheid)))))
            
        (update-label trein-snelheid label)))
    
    (define (update-locatie! treinidx)
      (let* ((detbl (get-treinlocatie treinidx))
             (label (~a "Detectieblok: " (if detbl
                                             detbl
                                             "niet gekend"))))
        (update-label trein-locatie label)))

    (define (update-treininformatie! trein-info)
      (when (thread? th)
        (kill-thread th))
      (set! th (thread (lambda ()
                         (start-timer!)
                         (teken-trein-informatie trein-info)
                         (let loop ()
                           (when (tijd-gepasseerd?)
                             (teken-trein-informatie trein-info))
                           ;   (sleep 1)
                           (loop))))))


    (define (update-snelheidsaanpasser! snelheid)
      (send trein-snelheidsaanpasser set-value (abs snelheid)))

    (define (update-vooruit/achteruit-selectie! vooruit?)
      (send rijd-vooruit-of-achteruit set-selection (if vooruit? 0 1)))

    ;; de informatie, knoppen en sliders van de geselecteerde trein weergeven
    (define (display-trein-informatie c e)
      (let ((gekozen-trein-idx (get-geselecteerde-id c)))
        (when (and gekozen-trein-idx
                   (not (eq? gekozen-trein-idx '|Kies een trein|))) ; toegevoegd om een domme linux bug op te lossen.
          (let((snelheid (get-treinsnelheid gekozen-trein-idx)))
            (update-treininformatie! gekozen-trein-idx)
            (update-snelheidsaanpasser! snelheid)
            (update-vooruit/achteruit-selectie! (get-treinrichting gekozen-trein-idx))
            (add-children panels (list verwijder-trein-knop
                                       trein-snelheid
                                       trein-locatie
                                       trein-snelheidsaanpasser
                                       rijd-vooruit-of-achteruit))))))
    
    (define (trein-snelheidsaanpasser-callback c e)
      (let* ((snelheid (send c get-value))
             (gekozen-trein-idx (get-geselecteerde-id treinen-keuzes)))
        (pas-treinsnelheid-aan! gekozen-trein-idx snelheid)
        (update-snelheid! gekozen-trein-idx)))

    ;; abstractie voor de callback van achteruit/vooruit rijden van de radio box 
    (define (vooruit-achteruit c e)
      (let ((vooruit-of-achteruit (send c get-selection))
            (gekozen-trein-idx (get-geselecteerde-id treinen-keuzes)))
        (if (= vooruit-of-achteruit vooruit-idx)
            (rijd-vooruit! gekozen-trein-idx)
            (rijd-achteruit! gekozen-trein-idx))))


    (define (append-trein-aan-de-treinen-keuzes trein-id)
      (send treinen-keuzes append trein-id))
      
      
    ;; bij het toevoegen van een trein verschijnt een nieuw venster die dan de gebruiker de informatie laat invullen
    (define (trein-toevoegen-callback c e)

      (let* ((treinen-toevoeging-window (maak-trein-toevoeging-window get-detectieblok-buren
                                                                      get-detectieblokken-ids
                                                                      voeg-trein-toe
                                                                      append-trein-aan-de-treinen-keuzes)))
        (treinen-toevoeging-window 'show)))


    (define (verwijder-trein-callback c e)
      (let* ((idx (send treinen-keuzes get-selection))
             (trein-id (string->symbol (send treinen-keuzes get-string idx))))
        (verwijder-trein trein-id)
        (send treinen-keuzes delete idx)
        (kill-thread th)
        (send panels change-children  (lambda (c*)
                                        (kies-tab treinen-tab)))))
                                       
    (define treinen-keuzes (new choice%
                                (label "Kies trein: ")
                                (parent panels)
                                (choices (if (eq? (system-type) 'unix)
                                             (cons "Kies een trein" (get-trein-ids))
                                             (get-trein-ids)))
                                (callback display-trein-informatie)))

    (define trein-snelheid (new message%
                                [parent panels]
                                [label "Snelheid: "]
                                [style '(deleted)]
                                [auto-resize #t]))
    
    (define trein-locatie (new message%
                               [parent panels]
                               [label "Locatie: "]
                               [style '(deleted)]
                               [auto-resize #t]))


    
    (define trein-snelheidsaanpasser (new slider%
                                          [label "Pas snelheid aan: "]
                                          [min-value min-snelheid]
                                          [max-value max-snelheid]
                                          [parent panels]
                                          [style '(deleted horizontal)]
                                          [callback trein-snelheidsaanpasser-callback]
                                          [min-width 50]
                                          [min-height 50]))

    



    (define rijd-vooruit-of-achteruit (new radio-box%
                                           [label "treinrichting: "]
                                           [choices (list "Vooruit"
                                                          "achteruit")]
                                           [parent panels]
                                           [style '(deleted horizontal)]
                                           [callback vooruit-achteruit]))
                                         
    


    (define voeg-trein-toe-knop (new button%
                                     [parent panels]
                                     [label "Voeg trein toe"]
                                     [callback trein-toevoegen-callback]))

    (define verwijder-trein-knop (new button%
                                      [parent panels]
                                      [label "Verwijder trein"]
                                      [style '(deleted)]
                                      [callback verwijder-trein-callback]))

    

    ;--------------------------- detectieblokken -------------------------------

    (define (update-aanwezige-treinid! detblidx)
      (let* ((trein-id (trein-aanwezig? detblidx))
             (label (if trein-id
                        (~a "Aanwezige trein: " trein-id)
                        "Er is geen trein aanwezig op dit blok.")))
        (send aanwezige-trein-op-detbl set-label label)))

    (define (update-detectieblok-informatie! detblidx)
      (when (thread? th)
        (kill-thread th))
      (set! th (thread (lambda ()
                         (start-timer!)
                         (update-aanwezige-treinid! detblidx)
                         (let loop ()
                           (when (tijd-gepasseerd?)
                             (update-aanwezige-treinid! detblidx))
                           ;(sleep 1)
                           (loop))))))
        

    (define (display-detectieblok-informatie c e)
      (let* ((gekozen-detectieblok-idx (get-geselecteerde-id c)))
        (update-detectieblok-informatie! gekozen-detectieblok-idx)
        (add-children panels (list aanwezige-trein-op-detbl))))
        
        
    (define detectieblokken-keuzes (new choice%
                                        (label "Kies detectieblok ")
                                        (parent panels)
                                        (choices (get-detectieblokken-ids))
                                        (style '(deleted))
                                        (callback display-detectieblok-informatie)))

    (define aanwezige-trein-op-detbl (new message%
                                          (label "laatste/huidige trein: ")
                                          (parent panels)
                                          (style '(deleted))
                                          (auto-resize #t)))
    
    ;------------------------------- wissels ------------------------------------


    (define (toon-wisselstand! stand)
      (let ((label (~a "Wisselstand: " stand)))
        (update-label wisselstand label)))

    (define (update-wisselstand-informatie! wisselstandidx)
      (let ((wisselstand (get-wisselstand wisselstandidx)))
        (toon-wisselstand! wisselstand)))

    (define (update-wissel-informatie! wisselstandidx)
      (when (thread? th)
        (kill-thread th))
      (set! th (thread (lambda ()
                         (start-timer!)
                         (update-wisselstand-informatie! wisselstandidx)
                         (let loop ()
                           (when (tijd-gepasseerd?)
                             (update-wisselstand-informatie! wisselstandidx))
                           ;  (sleep 1)
                           (loop))))))

    (define (display-wissel-informatie c e)
      (let* ((gekozen-wissel-idx (get-geselecteerde-id c)))
        (update-wissel-informatie! gekozen-wissel-idx)
        (add-children panels (list wisselstand wisselstand-aanpasser))))

    (define (wisselstand-aanpasser-callback c e)
      (let* ((wisselidx (get-geselecteerde-id wissels-keuzes)))
        (verander-wisselstand! wisselidx)))
      
    
    (define wissels-keuzes (new choice%
                                (label "Kies wissel ")
                                (parent panels)
                                (choices (get-wissels-ids))
                                (style '(deleted))
                                (callback display-wissel-informatie)))

    (define wisselstand (new message%
                             [parent panels]
                             [label "Wisselstand: "]
                             [style '(deleted)]
                             [auto-resize #t]))

            
    (define wisselstand-aanpasser (new button%
                                       [label "Pas wisselstand aan"]
                                       [parent panels]
                                       [style '(deleted)]
                                       [callback wisselstand-aanpasser-callback]))

    ;-------------------- tijdstabellen -------------------------


    (define (selecteer-tijdstabellen c e)
      (let* ((pad (get-file))
             (execution-status (laad-traject-in pad)))
        (unless execution-status
          (pop-up-error-window))))

    (define selecteer-tijdstabel-bericht   (new message%
                                                [parent panels]
                                                [label "Selecteer een bestand dat tijdstabellen bevat"]
                                                [style '(deleted)]
                                                [auto-resize #t]))
      
    (define selecteer-tijdstabellen-knop (new button%
                                              [parent panels]
                                              [label "selecteer een bestand"]
                                              [callback selecteer-tijdstabellen]
                                              [style '(deleted)]))


    ;; een tab selecteren afh van de naam van de tab (om de inhoud van de tab weer te geven)
    (define tabs (hash treinen-tab (list treinen-keuzes voeg-trein-toe-knop)
                       wissels-tab (list wissels-keuzes)
                       detectieblokken-tab (list detectieblokken-keuzes)
                       tijdstabellen-tab (list selecteer-tijdstabel-bericht selecteer-tijdstabellen-knop)))
    
    (define (kies-tab tab)
      (let ((gevonden-tab (hash-ref tabs tab #f)))
        (if gevonden-tab
            gevonden-tab
            (error "keuzemenu is niet gekend" tab))))

    (define (dispatch msg)
      (cond ((eq? msg 'show) (send frame show #t))
            ((eq? msg 'unshow) (send frame show #f)
                               (kill-thread th))
            (else (error "ongekend bericht" msg))))
    dispatch))