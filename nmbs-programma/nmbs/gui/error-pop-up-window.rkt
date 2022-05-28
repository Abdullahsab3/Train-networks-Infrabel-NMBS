#lang racket/gui
(require "../../shared-code/helpers/error-handling.rkt")
(require "../../shared-code/constanten.rkt")


(provide pop-up-error-window)

(define (pop-up-error-window)
  (let ((frame (new frame%
                    [label "Error!"]
                    [width venster-breedte]
                    [height error-pop-up-venster-hoogte])))


    (define error-bericht  (new message%
                                [parent frame]
                                [label (get-error)]
                                [auto-resize #t]))
    (define ok-knop   (new button%
                           [parent frame]
                           [label "OK"]
                           [callback (lambda (c e)
                                       (send frame show #f))]))


    (send frame show #t)))