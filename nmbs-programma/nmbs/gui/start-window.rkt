#lang racket/gui

(require "../../shared-code/constanten.rkt"
         "error-pop-up-window.rkt")


(provide maak-start-window)

(define (maak-start-window maak-connectie zoek-pi-ip start-opstelling-window! start-main-gui!) 
  (let ((start-frame (new frame%
                          [label "Verbinding"]
                          [width venster-breedte]
                          [height venster-hoogte])))
      
    (define welkom-bericht (new message%
                                [parent start-frame]
                                [label "Welkom in de GUI toepassing! \n"]
                                [vert-margin 20]
                                [auto-resize #t]))

      
    (define ip-input (new text-field%
                          [label "Geef het ip adres van Infrabel in"]	 
                          [parent start-frame]))
    (define zoek-automatisch-inleiding (new message%
                                            [parent start-frame]
                                            [label "Zoek het rpi IP-adres in het network"]
                                            [auto-resize #t]))
    
    (define zoek-automatisch-knop (new button%
                                       [label "Zoek in het netwerk!"]
                                       [parent start-frame]
                                       (callback (lambda (c e)
                                                   (let ((ipaddr (zoek-pi-ip)))
                                                     (if ipaddr
                                                         (send ip-input set-value ipaddr)
                                                         (pop-up-error-window)))))))

    (define (dien-verzoek-in c e)
      (let* ((ip (send ip-input get-value))
             (opstelling-status (maak-connectie ip)))
        
        (if opstelling-status
            (begin
              (send start-frame show #f)
              (cond ((eq? opstelling-status 'ingeladen) (start-main-gui!))
                    ((eq? opstelling-status 'geen-opstelling) (start-opstelling-window!))
                    (else (error "ongekend bericht"))))
            (pop-up-error-window))))
    
    (define ok-knop (new button%
                         [label "OK"]
                         [parent start-frame]
                         [callback dien-verzoek-in]))

                                       

    (define (stop)
      (if (send start-frame is-shown?)
          (send start-frame show #f)
          #f))
    
    (define (dispatch msg)
      (cond ((eq? msg 'show) (send start-frame show #t))
            ((eq? msg 'unshow) (stop))
            (else (error "ongekende boodschap" msg dispatch))))
    
    dispatch))
