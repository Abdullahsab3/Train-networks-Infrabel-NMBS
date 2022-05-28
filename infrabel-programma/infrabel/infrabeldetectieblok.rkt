#lang racket

(require "../shared-code/super-adts/superdetectieblok.rkt")


(provide maak-infrabeldetectieblok)

;; gebruikt momenteel alleen de operaties van zijn parent
(define (maak-infrabeldetectieblok id)
  (let ((parent (maak-superdetectieblok id)))


    (define (dispatch msg)
      (parent msg))

    dispatch))
