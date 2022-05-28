#lang racket

(require "nmbs/nmbs.rkt")

;; NMBS instantie maken
(define nmbs (maak-nmbs))
;; NMBS activeren
((nmbs 'start))

