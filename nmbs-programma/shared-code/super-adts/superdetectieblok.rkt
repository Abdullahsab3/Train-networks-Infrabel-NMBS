#lang racket

(require "superblok.rkt")

(provide maak-superdetectieblok)

(define (maak-superdetectieblok id)
  (let ((aanwezige-trein #f)
        (parent (maak-blok id)))

    (define (set-aanwezige-trein! trein)
      (set! aanwezige-trein trein))

    (define (dispatch msg)
      (cond ((eq? msg 'trein-aanwezig?) aanwezige-trein)
            ((eq? msg 'set-aanwezige-trein!) set-aanwezige-trein!)
            ((eq? msg 'id) id)
            
            (else (parent msg))))
    
    dispatch))