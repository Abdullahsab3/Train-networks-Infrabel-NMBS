#lang racket

(require "../constanten.rkt")
(provide maak-blok)


(define (maak-blok id)
  (let ((buren (make-vector 2 #f)))


    (define (set-buren! ids)
      (if (= (length ids) 2)
          (set! buren (list->vector ids))
          (error "de gegeven buren voor dit detectieblok kloppen niet")))

    (define (buur? id)
      (vector-member id buren))

    (define (get-begseg)
      (vector-ref buren begseg-idx))

    (define (get-eindseg)
      (vector-ref buren eindseg-idx))

    (define (true? el)
      (not (false? el)))

    (define (get-naburige-segmenten-ids)
      (map (lambda (el)
             (symbol->string el))
           (filter true?
                   (vector->list buren))))

    (define (dispatch msg)
      (cond ((eq? msg 'id) id)

            ((eq? msg 'set-buren!) set-buren!)
            ((eq? msg 'begseg) (get-begseg))
            ((eq? msg 'eindseg) (get-eindseg))
            ((eq? msg 'get-naburige-segmenten-ids) get-naburige-segmenten-ids)

            ((eq? msg 'buur?) buur?)

            
            (else (error "ongekende boodschap" msg))))
    
    dispatch))