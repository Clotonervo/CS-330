#lang racket


;-------------------------------------------- convertFC temps DONE
(define (convertFC temps)
  (map (lambda (x)
         (/ (* (- x 32) 5) 9)) temps)
  )

;-------------------------------------------- Check temps DONE
(define (check-temps temps low high)
    (andmap (lambda (lst)
            (if (empty? lst) true (and (not (positive? (- lst high))) (not (negative? (- lst low))))))
          temps)
  )

;-------------------------------------------- Check temps1 DONE
(define (check-temps1 temps)
  (if (empty? temps) true
  (check-temps temps 5 95))
  )

;-------------------------------------------- Convert digits DONE
(define (convert digits)
  (if (empty? digits) empty
  (foldr (lambda (x y)
           (+ x (* y 10))) 0 digits))
  )

;-------------------------------------------- Duple lst DONE
(define (duple lst)
  (map (lambda (x y)
         (append (list y) (list x))) lst lst)
  )

;-------------------------------------------- Average lst DONE
(define (average lst)
  (if (empty? lst) empty
  (/ (foldr + 0 lst) (length lst)))
  )

;-------------------------------------------- eliminate-larger lst ###TODO####
(define (is-smallest lst val)
  [cond [(empty? lst) #t]
        [(> (first lst) val) (is-smallest (rest lst) val) ]
        [else #f]]
  )

(define (eliminate-larger-helper lst)
  (if (is-smallest (rest lst) (first lst)) (cons (first lst) (eliminate-larger (rest lst))) (eliminate-larger (rest lst)))
  )

(define (eliminate-larger lst)
  (if (empty? lst) empty (eliminate-larger-helper lst))
  )

(define (elim-larger lst)
  (filter-map (lambda (x y)
            (is-smallest(y x)) lst lst)) 
  )

;(elim-larger (list 1 3 2))

;-------------------------------- curry2 func DONE

(define (curry2 func)
  (lambda (arg1)
    (lambda (arg2)
      (func arg1 arg2))))
