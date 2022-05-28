#lang racket

(require "../shared-code/spooropstellingen/segmenten-connecties.rkt"
         "../shared-code/super-adts/superspoormodel.rkt"
         "nmbswissel.rkt"
         "nmbsdetectieblok.rkt"
         "nmbsblok.rkt")

(provide maak-hardwarenmbsspoormodel)

(define (maak-hardwarenmbsspoormodel detectieblokken-ids wissels-ids)
  (let ((parent (maak-superspoormodel)))

    (define (initialiseer)
      ((parent 'initialiseer) detectieblokken-ids
                              maak-nmbsdetectieblok
                              wissels-ids
                              maak-nmbswissel
                              maak-nmbsblok
                              connecties-in-hardware))
    (initialiseer)
    parent))


; (define (dispatch msg)
;  (cond ((eq? msg 'initialiseer) initialiseer)
;       (else (parent msg))))
    
; dispatch))