#lang racket
(define in-S?
  (lambda (n)
    (if (zero? n) #t
        (let ((m (- n 3)) )
          (if (>= m 0) (in-S? m)
              #f)))))
