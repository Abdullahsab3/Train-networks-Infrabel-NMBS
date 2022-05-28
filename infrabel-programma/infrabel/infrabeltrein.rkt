#lang racket


(provide maak-trein)
(require "switch-simulator-hardware.rkt"
         "../shared-code/super-adts/supertrein.rkt")

(define (maak-trein id vorig-segment beginsegment #:switch-object [switch-object switch-object])
  (let ((parent (maak-supertrein id vorig-segment beginsegment)))

    ;; overridet de pas-snelheid-aan! procedure van de parent om met het spoormodel te communiceren.
    (define (pas-snelheid-aan! num)
      (switch-object 'set-loco-speed! id
                     (if (parent 'omgekeerd?)
                         (- num)
                         num))
      ((parent 'pas-snelheid-aan!) num))

    
    (define (update-snelheid!)
      (pas-snelheid-aan!  (abs (switch-object 'get-loco-speed id))))


    (define (rijd-achteruit!)
      ((parent 'rijd-achteruit!))
      (update-snelheid!))

    (define (rijd-vooruit!)
      ((parent 'rijd-vooruit!))
      (update-snelheid!))

    (define (reverse-richting!)
      (if (parent 'vooruit?)
          (rijd-achteruit!)
          (rijd-vooruit!)))
    
    (define (snelheid)
      (let ((snelheid (switch-object 'get-loco-speed id)))
        ((parent 'pas-snelheid-aan!) snelheid)
        snelheid))

    (define (update-locatie)
      (let ((detbl-id (switch-object 'get-loco-detection-block id)))
        (unless (or (eq? detbl-id (parent 'locatie))
                    (false? detbl-id))
          ((parent 'pas-locatie-aan!) detbl-id))
        detbl-id))

    
    (define (dispatch msg)
      (cond ((eq? msg 'pas-snelheid-aan!) pas-snelheid-aan!)
            ((eq? msg 'snelheid) (snelheid))
            ((eq? msg 'update-locatie) update-locatie)
            ((eq? msg 'rijd-vooruit!) rijd-vooruit!)
            ((eq? msg 'rijd-achteruit!) rijd-achteruit!)
            ((eq? msg 'reverse-richting!) reverse-richting!)
            (else (parent msg)))) 
    
    (switch-object 'add-loco id vorig-segment beginsegment)
    (update-locatie)
    dispatch))
