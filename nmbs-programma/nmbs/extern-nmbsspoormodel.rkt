#lang racket

(require "../shared-code/super-adts/super-externspoormodel.rkt"
         "nmbsdetectieblok.rkt"
         "nmbsblok.rkt"
         "nmbswissel.rkt"
         (prefix-in algemeen: "../shared-code/helpers/algemene-inleescode.rkt"))

(provide maak-extern-nmbsspoormodel)

(define (maak-extern-nmbsspoormodel definities ruwe-segmenten ruwe-connecties)
  (define (maak-object-args module poort extra id ignore)
    (list id))

  (define (welk-soort module poort extra defs)
    ((algemeen:welk-object maak-nmbsdetectieblok
                           maak-nmbsblok
                           maak-nmbswissel
                           maak-nmbswissel) module poort extra defs))

  (let ((parent (maak-externspoormodel definities
                                       ruwe-segmenten
                                       ruwe-connecties
                                       maak-object-args
                                       welk-soort)))
    parent))