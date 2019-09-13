#lang racket

;------------------ Notes for Higher order function

(define a (list 1 2 3 4 5))

(map (lambda (x) (+ x 1)) a) ; can also be a two parameter function and two list (stops with shortest list)

(filter even? a)             ; For any true and false tests that need to be imposed on a list
(filter odd? a)              ; Returns a list of all things that are true in the list

(foldr + 0 a)                ; First thing in the list and the folded rest of the list
(foldr * 1 a)

(foldr string-append "" (map number->string a))



