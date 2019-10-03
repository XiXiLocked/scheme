#lang eopl
(define identifier?
  (lambda (name)
    (and (symbol? name)
         (not (eqv? name 'lambda))
         )))
(define list-of
  (lambda (pred)
    (lambda (val)
      (or (null? val)
          (and (pair? val)
               (pred (car val))
               ((list-of pred) (cdr val))
               )))))

(define-datatype lc-exp lc-exp?
  (var-exp (var identifier?))
  (lambda-exp (bound-vars (list-of identifier?))
              (body lc-exp?))
  (app-exp (rator lc-exp?)
           (rands (list-of lc-exp?)))
  )

(define parser
  (lambda (datum)
    (cond
      ((symbol? datum) (var-exp datum))
      ((pair? datum)
       (if (eqv? (car datum) 'lambda)
           (lambda-exp
            (cadr datum)
            (parser (caddr datum)))
           (app-exp
            (parser (car datum))
            (map parser (cdr datum)))
           )))))

(define-datatype prefix-exp prefix-exp? (const-exp
                                         (num integer?))
  (diff-exp
   (operand1 prefix-exp?)
   (operand2 prefix-exp?)))

(define parser-prefix
  (lambda (prefix-list)
    (letrec ((proc (lambda (lst)
                     (if (null? lst)
                         lst
                         (if (integer? (car lst))
                             (list (const-exp (car lst)) (cdr lst))
                             (if (eqv? (car lst) '-)
                                 (let* ((op1 (proc (cdr lst)))
                                        (op2 (proc (cadr op1)))
                                        )
                                   (list (diff-exp (car op1) (car op2)) (cadr op2))
                                   )
                                 #f)
                             )))))
      (car (proc  prefix-list))
      )))


;exercise 2.29
(parser 'a)
(parser '(lambda (a) b))
(parser '(lambda (a b c) d))
(parser '(a b))
(parser '(a b c d))
(parser '(lambda (a b c) d))
(parser '((lambda (a b c) (a b c)) g h e))
;exercise 2.31
(parser-prefix '(- - 3 2 - 4 - 12 7))
(diff-exp
 (diff-exp
  (const-exp 3)
  (const-exp 2))
 (diff-exp
  (const-exp 4)
  (diff-exp
   (const-exp 12)
   (const-exp 7))))