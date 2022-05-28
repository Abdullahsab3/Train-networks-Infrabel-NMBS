#lang racket

(provide (all-defined-out))


(define venster-hoogte 300)
(define venster-breedte 300)

(define treinen-tab "Treinen")
(define wissels-tab "Wissels")
(define detectieblokken-tab "Detectieblokken")
(define tijdstabellen-tab "Tijdstabellen")

(define 2evenster-hoogte 200)
(define 2evenster-breedte 200)

(define error-pop-up-venster-hoogte 100)

(define max-aantal-treinen 20)
(define aantal-detectieblokken 50)
(define aantal-wissels 50)


(define min-snelheid 0)
(define max-snelheid 250)


(define snelheid-idx 1)
(define omgekeerd?-idx 0)


(define begseg-idx 0)
(define eindseg-idx 1)

(define vooruit-idx 0)
(define achteruit-idx 1)


(define trein-info-idx 0)
(define traject-idx 1)
(define timer-idx 2)


;; errors

(define non-existing-pikotrack 0)
(define unknown-procedure 1)
(define wrong-connections 2)
(define nonexisting-segment 3)
(define nonexisting-connection 4)
(define nonexiting-type 5)
(define empty-file 6)
(define empty-type-definition 7)
(define empty-segments-definition 8)
(define empty-connections 9)
(define wrong-setupformat 10)
(define file-not-provided 11)
(define no-routes 12)
(define no-end-db 13)
(define no-beg-db 14)
(define not-neighbors 15)
(define wrong-routeformat 16)
(define existing-train 17)
(define wrong-commando-place 18)
(define no-train-ID-provided 19)
(define no-speed-provided 20)
(define ip-not-found 21)
(define no-ip-provided 22)



(define executed #t)
(define failed #f)


(define analyseer-de-volgende #t)
(define analyseer-niet-verder #f)


(define infraport 7500)
(define simulport 7501)

(define SIMULATOR 0)
(define HARDWARE 1)

(define start-met-simulator #f)
(define start-met-hardware #t)