#lang racket

(require (prefix-in hardware: "../hardware-library/interface.rkt")
         (prefix-in simulator: "../simulator-library/interface.rkt")
         "../shared-code/tcp/sender.rkt"
         "../shared-code/constanten.rkt")

(provide switch-object set-switch-object!)
   
(define (switch-tussen-simulator/hardware hardware?)

  (define sender (if hardware? #f (maak-sender "localhost" simulport)))


  
  
  (define (kies-proc hardware-proc simulator-proc . args)
    (if hardware?
        (apply hardware-proc args)
        (apply simulator-proc args)))


  (define (laad-opstelling-in definities ruwe-segmenten ruwe-connecties)
    (kies-proc (lambda () #t)
               simulator:lees-opstelling-in definities
               ruwe-segmenten ruwe-connecties))
  
  (define (add-loco id previous-segment-id current-segment-id)
    (kies-proc hardware:add-loco
               simulator:add-loco
               id previous-segment-id current-segment-id))

  (define (remove-loco id)
    (kies-proc (lambda () #f)
               simulator:remove-loco id))
    
  (define (get-loco-speed id)
    (kies-proc hardware:get-loco-speed
               simulator:get-loco-speed
               id))

  (define (get-loco-detection-block id)
    (kies-proc hardware:get-loco-detection-block
               simulator:get-loco-detection-block
               id))

  (define (set-loco-speed! id speed)
    (kies-proc hardware:set-loco-speed!
               simulator:set-loco-speed!
               id speed))

  (define (get-switch-position id)
    (kies-proc hardware:get-switch-position
               simulator:get-switch-position
               id))

  (define (set-switch-position! id position)
    (kies-proc hardware:set-switch-position!
               simulator:set-switch-position!
               id position))

  (define (start)
    (kies-proc hardware:start
               simulator:start))

  (define (stop)
    (kies-proc hardware:stop
               simulator:stop))

  (define (setup-hardware)
    (kies-proc (lambda () #t)
               simulator:setup-hardware))

  (define (setup-loop)
    (kies-proc (lambda () #t)
               simulator:setup-loop))
  
  (define (setup-loop-and-switches)
    (kies-proc (lambda () #t)
               simulator:setup-loop-and-switches))

  (define (get-detection-block-ids)
    (kies-proc (lambda () #t)
               simulator:get-detection-block-ids))

  (define (get-switch-ids)
    (kies-proc (lambda () #t)
               simulator:get-switch-ids))

  (define (dispatch msg . args)
    (apply
     (cond ((eq? msg 'add-loco) add-loco)
           ((eq? msg 'remove-loco) remove-loco)
           ((eq? msg 'get-loco-speed) get-loco-speed)
           ((eq? msg 'set-loco-speed!) set-loco-speed!)
           ((eq? msg 'get-loco-detection-block) get-loco-detection-block)
           ((eq? msg 'get-switch-position) get-switch-position)
           ((eq? msg 'set-switch-position!) set-switch-position!)
           ((eq? msg 'get-detection-block-ids) get-detection-block-ids)
           ((eq? msg 'get-switch-ids) get-switch-ids)
           ((eq? msg 'start) start)
           ((eq? msg 'stop) stop)
           ((eq? msg 'setup-hardware) setup-hardware)
           ((eq? msg 'setup-loop) setup-loop)
           ((eq? msg 'setup-loop-and-switches) setup-loop-and-switches)
           ((eq? msg 'laad-opstelling-in) laad-opstelling-in)
           (else (error "onbekende boodschap" msg)))
     args))
  
  dispatch)


(define switch-object #f)

(define (set-switch-object! hardware?)
  (set! switch-object (switch-tussen-simulator/hardware hardware?))
  switch-object)
