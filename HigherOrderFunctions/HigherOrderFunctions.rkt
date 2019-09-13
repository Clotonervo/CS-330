#lang racket


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

;-------------------------------------------- Average lst ###TODO####
(define (average lst)
  (/ (foldr + 0 lst) (length lst))
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
