#lang racket

(require "infrabeldetectieblok.rkt"
         "infrabelwissel.rkt"
         "infrabeldriewegwissel.rkt"
         "infrabelblok.rkt"
         "../shared-code/super-adts/super-externspoormodel.rkt"
         (prefix-in algemeen: "../shared-code/helpers/algemene-inleescode.rkt"))


(provide maak-extern-infrabelspoormodel)

(define (maak-extern-infrabelspoormodel definities ruwe-segmenten ruwe-connecties)

  (define (maak-object-args module poort extra id ignore)
    (if (algemeen:extra? extra)
        (list id
              (maak-infrabelwissel (algemeen:to-id module poort))
              (maak-infrabelwissel (algemeen:to-id module extra)))
        (list id)))

  (define (welk-soort module poort extra defs)
    ((algemeen:welk-object maak-infrabeldetectieblok
                           maak-infrabelblok
                           maak-infrabelwissel
                           maak-driewegwissel) module poort extra defs))
  
  (let ((parent (maak-externspoormodel definities
                                       ruwe-segmenten
                                       ruwe-connecties
                                       maak-object-args
                                       welk-soort)))
    
    parent))