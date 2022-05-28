#lang racket

(require "../shared-code/super-adts/superspoormodel.rkt"
         "switch-simulator-hardware.rkt"
         "infrabeldetectieblok.rkt"
         "infrabelwissel.rkt"
         "infrabelblok.rkt"
         "infrabeldriewegwissel.rkt"
         "../shared-code/spooropstellingen/segmenten-connecties.rkt")

(provide maak-hardware-spoormodel)


(define (maak-hardware-spoormodel)
  ;; erft de operaties van het infrabelspoormodel
  (let ((parent (maak-superspoormodel)))

    (define (verander-wissel-naar-driewegwissel! wissel-id eerste-wissel-id tweede-wissel-id)
      (let ((oude-wissel ((parent 'vind-wissel) wissel-id))
            (wissel (maak-driewegwissel wissel-id
                                        (maak-infrabelwissel eerste-wissel-id)
                                        (maak-infrabelwissel tweede-wissel-id))))
        ((wissel 'set-buren!) (vector->list (oude-wissel 'get-buren)))
        
        ((parent 'pas-wissel-aan!) wissel-id
                                   wissel)))


    (define (initialiseer)
      ((parent 'initialiseer) (switch-object 'get-detection-block-ids)
                              maak-infrabeldetectieblok
                              (switch-object 'get-switch-ids)
                              maak-infrabelwissel
                              maak-infrabelblok
                              connecties-in-hardware)
      ;; de driewegwissel omzetten naar een proper driewegwisselobject
      (verander-wissel-naar-driewegwissel! 'S-2-3 'S-2 'S-3))

    (define (dispatch msg)
      (cond ((eq? msg 'initialiseer) initialiseer)
            (else (parent msg))))
    
    dispatch))
      
