#lang racket


(provide (all-defined-out))

;; algemene hulpfuncties die het gemakkelijk maken om een vector van objecten te maken en om een stringlijst van de ids van de objecten terug te geven.

(define (get-objs-ids objs)
  (map (lambda (id)
         (symbol->string id))
       (hash-keys objs)))

;; zal, gegeven een lijst van ids en een aanmaakoperatie van objecten, een hashtable maken en vullen met deze objecten.

(define (maak-objecten-uit-ids ids maak-obj)
  (let ((storage (make-hash)))
    (let loop ((lst ids))
      (if (null? lst)
          storage
          (let ((curr-id (car lst)))
            (hash-set! storage
                       curr-id
                       (maak-obj curr-id))
            (loop (cdr lst)))))))



;; wordt gebruikt om te zoeken in een vectorobject
;;(de implementatie van het vector ADT is een klein beetje generisch en het zoeken naar een id kan met behulp van een =? 
(define (=? id obj)
  (eq? id (obj 'id)))

