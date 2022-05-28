#lang racket/gui

(require "../../shared-code/constanten.rkt"
         "error-pop-up-window.rkt")

(provide maak-opstelling-selectie-window)

(define (maak-opstelling-selectie-window lees-opstelling-in
                                         start-hardware-opstelling!
                                         start-main-gui!)
  (let ((opstelling-frame (new frame%
                               [label "Kies opstelling"]
                               [width venster-breedte]
                               [height venster-hoogte])))
      
    (define welkom-bericht (new message%
                                [parent opstelling-frame]
                                [label "Welkom in de GUI toepassing! \nSelecteer een spooropstelling."]
                                [vert-margin 20]
                                [auto-resize #t]))

    (define (selecteer-opstelling c e)
      (let ((pad (get-file)))
        (let ((execution-status (lees-opstelling-in pad)))
          (cond (execution-status
                 (send opstelling-frame show #f)
                 (start-main-gui!))
                (else (pop-up-error-window))))))
      
    (define selecteer-bestand-knop (new button%
                                        [parent opstelling-frame]
                                        [label "selecteer een bestand"]
                                        [callback selecteer-opstelling]))

    (define selecteer-hardware-bericht (new message%
                                            [parent opstelling-frame]
                                            [label "of selecteer de hardware opstelling."]
                                            [vert-margin 20]
                                            [auto-resize #t]))

    (define (selecteer-hardware c e)
      (start-hardware-opstelling!)
      (send opstelling-frame show #f)
      (start-main-gui!))

    (define selecteer-hardware-knop (new button%
                                         [parent opstelling-frame]
                                         [label "start hardware opstelling!"]
                                         [callback selecteer-hardware]))
                                        

      (define (stop)
        (if (send opstelling-frame is-shown?)
            (send opstelling-frame show #f)
            #f))
    
      (define (dispatch msg)
        (cond ((eq? msg 'show) (send opstelling-frame show #t))
              ((eq? msg 'unshow) (stop))
              (else (error "ongekende boodschap" msg dispatch))))
    
      dispatch))
  