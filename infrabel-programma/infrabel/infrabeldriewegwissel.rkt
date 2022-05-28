#lang racket

(provide maak-driewegwissel)

;; "ingang" van tweede wissel bevindt zich aan de 2e "uitgang" van de eerste wissel
(define (maak-driewegwissel id eerste-wissel tweede-wissel)
  (let ((stand 1))

    ;; heeft drie wisselstanden. 
    (define (pas-wisselstand-aan! nieuw)
      (cond ((= nieuw 1)
             ((eerste-wissel 'pas-wisselstand-aan!) 1))
            ((= nieuw 2)
             ((eerste-wissel 'pas-wisselstand-aan!) 2)
             ((tweede-wissel 'pas-wisselstand-aan!) 1))
            ((= nieuw 3)
             ((eerste-wissel 'pas-wisselstand-aan!) 2)
             ((tweede-wissel 'pas-wisselstand-aan!) 2))))

    (define (set-buren! buren-ids)
      (cond ((= (length buren-ids) 4)
             ((eerste-wissel 'set-buren!) (list (car buren-ids)
                                                (cadr buren-ids)
                                                (tweede-wissel 'id)))
             ((tweede-wissel 'set-buren!) (list (eerste-wissel 'id)
                                                (caddr buren-ids)
                                                (cadddr buren-ids))))
            
            (else (error "buren kloppen niet"))))

    (define (buur? id)
      (let ((buur-van-eerste-wissel? ((eerste-wissel 'buur?) id)))
        (cond (buur-van-eerste-wissel? buur-van-eerste-wissel?)
              (else (let ((buur-van-tweede-wissel? ((tweede-wissel 'buur?) id)))
                      (if buur-van-tweede-wissel?
                          (+ 1 buur-van-tweede-wissel?)
                          #f))))))

    (define (pas-wisselstand-aan-volgens-buur! buur-id)
      (let ((buur-idx (buur? buur-id)))
        (if (and buur-idx (> buur-idx 0))
            (pas-wisselstand-aan! buur-idx)
            buur-idx)))


    ;; zal de wisselstand updaten afh van de twee wissels.
    (define (update-wisselstand!)
      (let ((wiselstand-eerste-wissel (eerste-wissel 'wisselstand))
            (wisselstand-tweede-wissel (tweede-wissel 'wisselstand)))
        (cond ((= wiselstand-eerste-wissel 1)
               (set! stand 1))
              ((= wiselstand-eerste-wissel 2)
               (if (= wisselstand-tweede-wissel 1)
                   (set! stand 2)
                   (set! stand 3)))))
      stand)

    ;; gaat de wisselstand updaten naar de volgende wisselstand
    (define (verander-wisselstand!)
      (update-wisselstand!)
      (cond ((or (= stand 1) (= stand 2))
             (pas-wisselstand-aan! (+ stand 1)))
            (else (pas-wisselstand-aan! 1))))

    (define (dispatch msg)
      (cond ((eq? msg 'wisselstand) (update-wisselstand!))
            ((eq? msg 'pas-wisselstand-aan!) pas-wisselstand-aan!)
            ((eq? msg 'verander-wisselstand!) verander-wisselstand!)
            ((eq? msg 'buur?) buur?)
            ((eq? msg 'set-buren!) set-buren!)
            ((eq? msg 'pas-wisselstand-aan-volgens-buur!) pas-wisselstand-aan-volgens-buur!)
            ((eq? msg 'id) id)
            (else (error "ongekende boodschap" msg))))

    
    dispatch))
      
      

    
    
