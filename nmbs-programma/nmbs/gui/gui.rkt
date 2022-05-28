#lang racket/gui

(require "main-window.rkt"
         "start-window.rkt"
         "opstellingen-selectie-window.rkt")

(provide maak-gui)

(define (maak-gui

         
         get-ids

         lees-opstelling-in
         
         start-hardware-opstelling!

         ;; een callback om een trein toe te voegen
         voeg-trein-toe
         ;; een callback om een trein te verwideren
         verwijder-trein
         ;; een callback om de informatie van de trein op te vragen (inclusief de trein callbacks)
         trein-informatie-en-callbacks

         ;; een callback om de informatie van de detectieblokken op te vragen.
         detectieblokken-informatie

         wissels-informatie-en-callbacks
         
         laad-traject-in

         maak-connectie
         zoek-pi-ip)


  (let ((main-window #f)
        (opstelling-window #f))
    (define (start-opstelling-window!)
      (set! opstelling-window (maak-opstelling-selectie-window lees-opstelling-in
                                                               start-hardware-opstelling!
                                                               set-main-window!))
      (opstelling-window 'show))
    (define (set-main-window!)
      (set! main-window (maak-main-gui get-ids
                                       voeg-trein-toe
                                       verwijder-trein
                                       trein-informatie-en-callbacks
                                       detectieblokken-informatie
                                       wissels-informatie-en-callbacks
                                       laad-traject-in))
      (main-window 'show))
    
    (let ((start-window (maak-start-window maak-connectie
                                           zoek-pi-ip
                                           start-opstelling-window!
                                           set-main-window!)))
    
      (define (start)
        (start-window 'show))

      (define (stop)
        (unless (start-window 'unshow)
          (if (and main-window
                   (main-window 'is-shown?))
              (main-window 'unshow)
              #f)))


      (define (dispatch msg)
        (cond ((eq? msg 'start) (start))
              ((eq? msg 'stop) (stop))
              (else (error "ongekend bericht" msg))))
    
      dispatch)))

