#lang racket
(require "tracks.rkt")

(provide piko-tracks find-piko-track)

;----------------- Straight tracks ----------------

(define G940   (make-object straight-track% 940))

(define G239   (make-object straight-track% 239.07))

(define G231   (make-object straight-track% 230.93))

(define G119   (make-object straight-track% 119.54))

(define G115   (make-object straight-track% 115.46))

(define G107   (make-object straight-track% 107.32))

(define G62    (make-object straight-track% 61.88))

;-------------------- curved tracks ------------------
(define R1 (make-object curved-track% 360 (/ pi 6)))

(define R2     (make-object curved-track% 421.88 (/ pi 6)))

(define R2-7.5 (make-object curved-track% 421.88 (/ pi 24)))

(define R3     (make-object curved-track% 483.75 (/ pi 6)))

(define R9     (make-object curved-track% 907.97 (/ pi 12)))

;---------------- switches & crossings ---------------

(define WL     (make-object switch-track% (list G239 (invert R9))))

(define WR     (make-object switch-track% (list G239 R9)))

(define BWL    (make-object switch-track% (list (invert R2) (list G62 (invert R2)))))

(define BWR    (make-object switch-track% (list R2 (list G62 R2))))

(define W3     (make-object switch-track% (list R9 G239 (invert R9))))

(define DKWR   (make-object switch-track% (list (make-object straight-track% (/ 239.07 2)) (make-object curved-track% (/ 907.97 2) (/ pi 12)))))



(define piko-tracks (hash 'G940 G940
                          'G239 G239
                          'G231 G231
                          'G119 G119
                          'G115 G115
                          'G107 G107
                          'G62 G62

                          'R1 R1
                          'R2 R2
                          'R2-7.5 R2-7.5
                          'R3 R3
                          'R9 R9

                          'WL WL
                          'WR WR
                          'BWL BWL
                          'BWR BWR
                          'W3 W3
                          'DKWR DKWR))

(define (find-piko-track symbol)
  (hash-ref piko-tracks symbol #f))