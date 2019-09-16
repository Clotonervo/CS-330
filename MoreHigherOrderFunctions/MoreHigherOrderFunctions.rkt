#lang racket

(define (add x y)
  (+ x y)
  )

;--------------------------- default-parms DONE
(define (default-parms f values)
  (lambda args
    (if (eq? (length args) (length values)) (apply f args) (apply f (append args (list-tail values (length args))))))
  )

;----------------------- type-parms f types DONE
(define (type-checker values types)
  (if ((first types) (first values)) (type-checker-start (rest values) (rest types)) #f)
  )

(define (type-checker-start values types)
  (if (or (empty? values) (empty? types)) #t (type-checker values types))
  )

(define (type-parms f types)
  (lambda args
    (if (and (eq? (length args) (length types)) (type-checker-start args types)) (apply f args) ((error "ERROR MSG")))
  ))

;---------------------- new-sin f f
(define(degrees-to-radians angle)
  (define radian angle)
  (set! radian (/ radian 180))
  (set! radian (* radian pi))
  radian
)

(define (new-sin angle type)
  (define result null)
  (cond
    [(symbol=? type 'degrees) (set! result (sin (degrees-to-radians angle)))]
    [(symbol=? type 'radians) (set! result (sin angle))]
    )
  result)

(define new-sin2 (default-parms (type-parms new-sin (list number? symbol?)) (list 0 'radians)))
 
