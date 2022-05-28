#lang racket

(provide maak-supertrein)
(require "../constanten.rkt")

(define (maak-supertrein id vorig-segment beginsegment)
  (let ((richting-en-snelheid (vector #f 0))
        (vooruit #f)
        (detectieblok #f))

    (define (snelheid)
      (vector-ref richting-en-snelheid snelheid-idx))

    (define (omgekeerd?)
      (vector-ref richting-en-snelheid omgekeerd?-idx))

    (define (rijd-vooruit!)
      (unless vooruit
        (set! vooruit #t)
        (pas-rijrichting-aan!)))

    (define (rijd-achteruit!)
      (when vooruit
        (set! vooruit #f)
        (pas-rijrichting-aan!)))

     (define (pas-snelheid-aan! num)
      (vector-set! richting-en-snelheid snelheid-idx num))

     (define (pas-rijrichting-aan!)
      (vector-set! richting-en-snelheid omgekeerd?-idx (not (omgekeerd?))))

    (define (pas-detectieblok-aan! detblok)
      (set! detectieblok detblok))


    (define (dispatch msg)
      (cond ((eq? msg 'snelheid) (snelheid))
            ((eq? msg 'pas-snelheid-aan!) pas-snelheid-aan!)
            ((eq? msg 'locatie) detectieblok)
            ((eq? msg 'pas-locatie-aan!) pas-detectieblok-aan!)
            ((eq? msg 'id) id)
            ((eq? msg 'omgekeerd?) (omgekeerd?))
            ((eq? msg 'vooruit?) vooruit)
            ((eq? msg 'rijd-vooruit!) rijd-vooruit!)
            ((eq? msg 'rijd-achteruit!) rijd-achteruit!)
            ((eq? msg 'richting-en-snelheid) richting-en-snelheid)
            ((eq? msg 'pas-rijrichting-aan!) pas-rijrichting-aan!)
            (else (error "ongekende boodschap" msg))))
    
    
    dispatch))