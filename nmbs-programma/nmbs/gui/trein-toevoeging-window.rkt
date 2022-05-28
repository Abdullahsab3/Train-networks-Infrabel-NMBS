#lang racket/gui


(require "../../shared-code/constanten.rkt"
         (prefix-in error-handler: "../../shared-code/helpers/error-handling.rkt")
         "error-pop-up-window.rkt")

(provide maak-trein-toevoeging-window)

(define (maak-trein-toevoeging-window get-detectieblok-buren
                                      get-detectieblokken-ids
                                      stuur-trein
                                      append-trein-aan-keuzes)
                                      

  (define (get-geselecteerde-id keuzes)
    (string->symbol (send keuzes get-string-selection)))

  (define (update-keuzes keuzesobject keuzes)
    (send keuzesobject clear)
        
    (let loop ((lst keuzes))
      (unless (null? lst)
        (let ((keuze (car lst)))
          (send keuzesobject append keuze))
        (loop (cdr lst)))))
  
  (define (get-geselecteerde-detectieblok-buren keuzes)
    (let ((id (get-geselecteerde-id keuzes)))
      (get-detectieblok-buren id)))

  (let* ((input-frame (new frame%
                           [label "Voeg een trein toe"]
                           [width 2evenster-breedte]
                           [height 2evenster-hoogte])))

    (define (beginsegment-keuzes-callback c e)
      (let* ((detblid (get-geselecteerde-id c))
             (buren (get-detectieblok-buren detblid)))
        (update-keuzes vorigsegment-keuze buren)))
             
    (define beginsegment-keuze (new choice%
                                    [label "Kies een beginsegment"]
                                    [parent input-frame]
                                    [choices (get-detectieblokken-ids)]
                                    [callback beginsegment-keuzes-callback]))
             
    (define vorigsegment-keuze (new choice%
                                    [label "Kies een volgendsegment"]
                                    [parent input-frame]
                                    [choices (get-geselecteerde-detectieblok-buren beginsegment-keuze)]))

             
    (define trein-id-input (new text-field%
                                [label "Geef de trein ID in"]	 
                                [parent input-frame]))
             
    (define snelheid-input (new text-field%
                                [label "Geef de snelheid in"]	 
                                [parent input-frame]))

    (define (dien-treinaanmaakverzoek-in c e)
      (let* ((beginsegment (get-geselecteerde-id beginsegment-keuze))
             (eindsegment (get-geselecteerde-id vorigsegment-keuze))
             (trein-id (send trein-id-input get-value))
             (trein-id-symbol (if (non-empty-string? trein-id)
                                  (string->symbol trein-id)
                                  (error-handler:raise-error no-train-ID-provided)))
             (input-snelheid (let ((ruwe-snelheid-input (send snelheid-input get-value)))
                               (if (non-empty-string? ruwe-snelheid-input)
                                   (string->number (send snelheid-input get-value))
                                   (error-handler:raise-error no-speed-provided))))
             (snelheid (if input-snelheid
                           (if (< min-snelheid input-snelheid max-snelheid)
                               input-snelheid
                               min-snelheid)
                           #f)))   
            
        (if (and trein-id-symbol
                 snelheid
                 (stuur-trein trein-id-symbol
                              snelheid
                              beginsegment
                              eindsegment))
            (begin
              (append-trein-aan-keuzes trein-id)
              (send input-frame show #f))
            (pop-up-error-window))))
                
                
          
             
    (define ok-knop (new button%
                         [label "OK"]
                         [parent input-frame]
                         [callback dien-treinaanmaakverzoek-in]))

    (define (dispatch msg)
      (cond ((eq? msg 'show) (send input-frame show #t))
            ((eq? msg 'unshow) (send input-frame show #t))
            (else (error "ongekende boodschap" msg dispatch))))
        
    dispatch))