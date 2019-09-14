#lang racket
(define number-elements-from
  (lambda (lst n)
    (if (null? lst)
        '()
        (cons (list n (car lst))
              (number-elements-from (cdr lst) (+ n 1))))))
(define number-elements
  (lambda (lst)
    (number-elements-from lst 0)))

                    
    
    