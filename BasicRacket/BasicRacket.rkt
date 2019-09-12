#lang racket

;FINISHED! Check and find out how to run it with the python script
(define (sum-coins pennies nickels dimes quarters)
  (define total 0)
  (set! total (+ pennies total))
  (set! total (+ total (* 5 nickels)))
  (set! total (+ total (* 10 dimes)))
  (set! total (+ total (* 25 quarters)))
  total
)

;FINISHED! Check with python script
(define(degrees-to-radians angle)
  (define radian angle)
  (set! radian (/ radian 180))
  (set! radian (* radian pi))
  radian
)

;FINISHED!
(define (sign x)
  (define result 0)
  (if (positive? x) (set! result 1) null)
  (if (negative? x) (set! result -1) null)
  (if (zero? x) (set! result 0) null)
  result
)

;FINISHED!
(define (new-sin angle type)
  (define result null)
  (cond
    [(symbol=? type 'degrees) (set! result (sin (degrees-to-radians angle)))]
    [(symbol=? type 'radians) (set! result (sin angle))]
    )
  result
)