#lang racket
(require "../constanten.rkt"
         (prefix-in error-handler: "error-handling.rkt"))

(provide block?
         switch?
         extra?
         to-id
         welk-object
         maak-object
         decodeer-track
         decodeer-tracks)


(define (to-id module port . extra)
  (string->symbol
   (apply ~a (cons module
                   (cons '-
                         (cons port
                               (cond ((null? extra) extra)
                                     ((extra? (car extra))
                                      (cons '- extra))
                                     (else '()))))))))


(define (extra? extra)
  (number? extra))

(define (element? el def)
  (if (pair? def)
      (member el def)
      (eq? el def)))

(define (block? el definities)
  (let ((fst (car definities)))
    (if (element? el fst)
        #t
        (number? el))))

(define (undefined-block? el definities)
  (let ((sec (cadr definities)))
    (element? el sec)))

(define (switch? el definities)
  (let ((thrd (caddr definities)))
    (element? el thrd)))

(define (welk-object block undefined-block switch threewayswitch)
  (lambda (module poort extra defs)
    (cond ((undefined-block? module defs) undefined-block)
          ((block? module defs) block)
          ((switch? module defs) (if (extra? extra)
                                     threewayswitch
                                     switch))
          (else (error-handler:raise-error nonexiting-type module)))))


(define (maak-object construuer-args welk-object maak-op)
  (lambda (module poort extra id rest defs)
    (let ((obj (welk-object module poort extra defs))
          (object-args (construuer-args module poort extra id rest)))
      (if (and obj object-args)
          (apply maak-op obj object-args)
          failed))))
  
(define (decodeer-track lst maak-object voeg-toe! defs)
  (let* ((module (car lst))
         (poort (cadr lst))
         (extra (caddr lst))
         (id (to-id module poort extra))
         (rest (if (extra? extra) (cdddr lst) (cddr lst)))
         (object (maak-object module poort extra id rest defs)))
    (if object
        (voeg-toe! id module poort extra object)
        failed)))

; til dat racket pattern matching ondersteunt (had ik eerder moeten weten :( )
(define (check-definitions defs)
  (match defs
    [(list a b c) #t]
    [else #f]))

(define (decodeer-tracks lsts maak-object voeg-toe! defs)
  (if (check-definitions defs)
      (let ((execution-status executed))
        (let loop ((curr lsts))
          (unless (null? curr)
            (if (decodeer-track (car curr) maak-object voeg-toe! defs)
                (loop (cdr curr))
                (set! execution-status failed)))
          execution-status))
      (error-handler:raise-error empty-type-definition)))



