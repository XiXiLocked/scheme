#lang racket
(require eopl)
(define (list-length l)
  (if (null? l) 0
      (+ 1 (list-length (cdr l)))))

(define (nth-element l n)
  (if (null? l) (report-list-too-short n)
      (if (= n 0) (car l)
          (nth-element (cdr l) (- n 1)))))

(define report-list-too-short
  (lambda (n)
    (eopl:error 'nth-element "List too short by ~s elements.~%" (+ n 1))))

(define (nth-element-r l n)
  (define nth-element
    (lambda (l n)
      (if (null? l) (report-list-too-short-r)
          (if (= n 0) (car l)
              (nth-element (cdr l) (- n 1))))))
  (define report-list-too-short-r
    (lambda ()
      (eopl:error 'nth-element-r "~s does not have ~s element(s).~%" l n)))
  (nth-element l n))


(define (remove-first s loc)
  (if (null? loc) '()
      (let ((a (car loc))
            (b (cdr loc)))
        (if (eqv? a s) b
            (cons a (remove-first s b))))))

(define (remove-first-1.8 s loc)
  (if (null? loc) '()
      (let ((a (car loc))
            (b (cdr loc)))
        (if (eqv? a s) b
            (remove-first-1.8 s b)))))

(define (remove s loc)
  (if (null? loc) '()
      (let ((a (car loc))
            (b (cdr loc)))
        (if (eqv? a s) (remove s b)
            (cons a (remove s b))))))

(define occurs-free?
  (lambda (var exp)
    (cond ((symbol? exp) (eqv? var exp))
          ((eqv? (car exp) 'lambda)
               (let ((id (caadr exp))
                     (body (caddr exp)))
               (and
                (not (eqv? var id))
                (occurs-free? var body))))
          (else (let ((exp1 (car exp))
                      (exp2 (cadr exp)))
                  (or (occurs-free? var exp1)
                      (occurs-free? var exp2)))))))


(define test_occurs-free?
  (lambda ()
    (list 
      (occurs-free? 'x 'x) ;#t
      (occurs-free? 'x 'y) ;#f
      (occurs-free? 'x '(lambda (x) (x y))) ;#f
      (occurs-free? 'x '(lambda (y) (x y))) ;#t
      (occurs-free? 'x '((lambda (x) x) (x y))) ;#t
      (occurs-free? 'x '(lambda (y) (lambda (z) (x (y z))))) ;#t
      )))

(define (subst new old slist)
   (letrec ((subst-in-s-list (lambda (slist)
                               (if (null? slist)
                                   '()
                                   (cons
                                    (subst-in-s-exp (car slist))
                                    (subst-in-s-list (cdr slist))))))
            (subst-in-s-exp (lambda (sexp)
                              (if (symbol? sexp)
                                  (if (eqv? sexp old) new sexp)
                                  (subst-in-s-list sexp))))
            )
     (subst-in-s-list slist)))

(define (subst-1.12 new old slist)
  (define subst-in-s-list
           (lambda (slist)
             (if (null? slist)
                 '()
                 (cons
                  (let ((sexp (car slist)))
                    (if (symbol? sexp)
                        (if (eqv? sexp old) new sexp)
                        (subst-in-s-list sexp)))
                  (subst-in-s-list (cdr slist))))))
  (subst-in-s-list slist))

(define subst-1.13
  (lambda (new old slist)
    (letrec ((subst-in-s-list
              (lambda (slist) (map subst-in-s-exp slist)))
             (subst-in-s-exp
              (lambda (sexp)
                (if (symbol? sexp)
                        (if (eqv? sexp old) new sexp)
                        (subst-in-s-list sexp)))))
      (subst-in-s-list slist))
    )
  )
  
;(subst-1.13 'a 'b '( b b b))