#lang racket


;-------------------------------------------- Check temps DONE
(define (check-temps temps low high)
    (andmap (lambda (lst)
            (if (empty? lst) true (and (not (positive? (- lst high))) (not (negative? (- lst low))))))
          temps)
  )

;-------------------------------------------- Check temps1 DONE
(define (check-temps1 temps)
  (check-temps temps 5 95)
  )

;-------------------------------------------- Convert digits ###TODO####
(define (digit-helper digits power)
  (if (empty? digits) 0 (+ (digit-helper (rest digits) (+ power 1)) (* (first digits) (expt 10 power))))
  )

(define (convert digits)
  (foldr + 0 digits)
  
  )

(define (thing digits)
  (foldr
   (lambda (a b result) (+ a (* 10 result)))
   0
   digits
   digits))

(displayln "Convert digits tests")
(thing (list 1 2 3))
(convert (list 1 2 3 4 5))
;-------------------------------------------- Duple lst ###TODO####
(define (duple-helper lst)
    (if (empty? (rest lst)) (cons (list (first lst) (first lst)) empty) (cons (list (first lst) (first lst)) (duple (rest lst))))
  )

(define (duple lst)
  (if (empty? lst) empty (duple-helper lst))
  )

;-------------------------------------------- Average lst ###TODO####
(define (list-sum lst)
  (if (empty? lst) 0 (+ (first lst) (list-sum (rest lst))))
  )

(define (average lst)
  (if (empty? lst) empty (/ (list-sum lst) (length lst)))
  )

;-------------------------------------------- convertFC temps ###TODO####
(define (convertFC-helper temps)
   (if (empty? temps) empty (cons (/ (* (- (first temps) 32) 5) 9) (convertFC-helper (rest temps))))
  )

(define (convertFC temps)
  (if (empty? temps) empty (convertFC-helper temps))
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

;-------------------------------------------- get-nth lst n ###TODO####
(define (get-nth-helper lst n index)
  [cond ((empty? lst) -1)
        ((eq? n index) (first lst))
        (else (get-nth-helper (rest lst) n (+ index 1)))]
  )

(define (get-nth lst n)
  (get-nth-helper lst n 0)
  )

;-------------------------------------------- find-item lst target ###TODO####
(define (find-item-helper lst n index)
  [cond ((empty? lst) -1)
        ((eq? (first lst) n) index)
        (else (find-item-helper (rest lst) n (+ index 1)))]
  )

(define (find-item lst n)
  (find-item-helper lst n 0)
  )
