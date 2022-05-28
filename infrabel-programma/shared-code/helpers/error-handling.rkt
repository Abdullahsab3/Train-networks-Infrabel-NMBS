#lang racket


(provide get-error
         raise-error
         error?
         error-to-CLI)

(define errors (vector "piko track bestaat niet"
                       "ongekende procedure"
                       "Segmenten zijn niet correct geconnecteerd met elkaar"
                       "segment bestaat niet"
                       "connectie van het segment bestaat niet"
                       "segment type bestaat niet"
                       "bestand is leeg of bestaat niet"
                       "Definities van de types segmenten worden niet fegeven"
                       "Definities van de segmenten worden niet gegeven"
                       "connecties worden niet in het bestand gedefinieerd"
                       "Foutief opstellingformaat"
                       
                       "Gelieve een bestand te selecteren"
                       
                       "Er zijn geen trajecten gedefinieerd"
                       "het traject moet eindigen met een detectieblok"
                       "het traject moet met een detectieblok beginnen"
                       "gegeven segmenten zijn geen buren"
                       "foutief trajectformaat"
                       "trein bestaat al"
                       "Een commando kan alleen na een detectieblok komen"

                       "Gelieve een trein ID te geven"
                       "Gelieve de snelheid van de trein te geven"

                       "het IP adres van de raspberrypi werd niet gevonden"
                       "Gelieve een correct IP adres in te geven"))

(define raised-error #f)
(define raised-error-extra '())

(define error? number?)


(define (raise-error errorno . extra)
  (unless raised-error
    (set! raised-error errorno)
    (unless (null? extra)
      (set! raised-error-extra extra)))
  #f)


(define (list->string-comma-sep lst)
  (string-join
   (map (lambda (el)
          (~a el))
        lst)
   ", "
   #:after-last ": "))


(define (execution-succeded? sth)
  (eq? sth #t))

(define (return-error errorno extra)
  (if (null? extra)
      (vector-ref errors errorno)
      (~a (list->string-comma-sep extra) (vector-ref errors errorno))))

(define (get-error)
  (when raised-error
    (let ((errorno raised-error))
      (set! raised-error #f)
      (return-error errorno raised-error-extra))))

(define (error-to-CLI)
  (let ((errorno raised-error))
    (displayln (get-error))
    errorno))
                      