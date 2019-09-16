#lang racket

;------------------ Decorators
(define (add x y)
  (+ x y))

(define (debug f label)
  (lambda (x y)
    (printf "Called ~s: ~a ~a ~n" label x y)
    (f x y)))

(define add-d (debug add "add"))

(add-d 3 4)

(define add-all
  (lambda args
    (apply + args)))

(add-all 1 2 3 4 5)



