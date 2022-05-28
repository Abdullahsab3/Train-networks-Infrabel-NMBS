#lang racket

(provide maak-superwissel)

(define (maak-superwissel id)
  (let ((wisselstand 1)
        (buren (make-vector 3)))


    (define (set-buren! buren-ids)
          (set! buren (list->vector buren-ids)))
    

    (define (buur? id)
      (vector-member id buren))

    (define (pas-wisselstand-aan! nieuw)
      (set! wisselstand nieuw))



    (define (dispatch msg)
      (cond ((eq? msg 'wisselstand) wisselstand)
            ((eq? msg 'pas-wisselstand-aan!) pas-wisselstand-aan!)
            ((eq? msg 'set-buren!) set-buren!)
            ((eq? msg 'get-buren) buren)
            ((eq? msg 'id) id)
            ((eq? msg 'buur?) buur?)
            (else (error "ongekende boodschap" msg))))

    dispatch))