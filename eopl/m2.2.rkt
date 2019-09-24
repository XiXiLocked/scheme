#lang racket
(require eopl)

(define empty-env/list
  (lambda () (list'empty-env))
  )

(define extend-env/list
  (lambda (var val env)
    (list 'extend-env var val env))
  )

(define apply-env/list
  (lambda (env search-var)
    (cond
      ((eqv? (car env) 'empty-env)
       (report-no-binding-found search-var))
      ((eqv? (car env) 'extend-env)
       (let ((saved-var (cadr env))
             (saved-val (caddr env))
             (saved-env (cadddr env)))
         (if (eqv? search-var saved-var)
             saved-val
             (apply-env/list saved-env search-var))))
      (else (report-invalid-env env)))
    ))
(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "Nobinding for ~s" search-var)))

(define report-invalid-env
  (lambda (env) (eopl:error 'apply-env "Bad environment: ~s" env)
    ))


(define empty-env/a-list
  (lambda () '())
  )
(define extend-env/a-list
  (lambda (var val env)
    (cons (list var val) env)
    ))

(define apply-env/a-list
  (lambda (env search-var)
    (cond
      ((null? env) (report-no-binding-found search-var))
      (else (let ((saved-var (caar env))
                  (saved-val (cadar env))
                  (saved-env (cdr env)))
              (if (eqv? search-var saved-var)
                  saved-val
                  (apply-env/a-list saved-env search-var))))
      )))

(define empty-env?/a-list
  (lambda (env) (null? env))
  )

(define has-binding?/a-list
  (lambda (env search-var)
     (if (empty-env?/a-list env) #f
         (let ((saved-var (caar env))
               (saved-env (cdr env)))
              (if (eqv? search-var saved-var)
                  #t
                  (has-binding?/a-list saved-env search-var))))
      ))

(define extend-env*/a-list
  (lambda (list_var list_val env)
    (if (null? list_var)
        env
        (extend-env/a-list (car list_var) (car list_val)
                    (extend-env*/a-list (cdr list_var) (cdr list_val) env))
        )))

(define empty-env/ribs
  (lambda () '())
  )

(define extend-env/ribs
  (lambda (var val env)
    (cons (list (list var) (list val) ) env)
    ))

(define apply-env/ribs
  (lambda (env search-var)
    (if (empty-env?/ribs env)
        (report-no-binding-found search-var)
        (let* ((var-list (caar env))
               (val-list (cadar env))
               (saved-env (cdr env))
               (result (search search-var var-list val-list)))
          (if (not (null? result))
              (car result)
              (apply-env/ribs saved-env search-var)
              )))))

(define search
  (lambda (search-var var val)
    (if (null? var) val
        (if (eqv? search-var (car var))
            val
            (search search-var (cdr var) (cdr val))
            ))))
(define empty-env?/ribs
  (lambda (env)
    (null? env)
    ))
(define has-binding?/ribs
    (lambda (env search-var)
      (if (empty-env?/ribs env) #f
          (let ((varlist (caar env))
                (saved-env (cdr env)))
            (if (in-varlist? varlist search-var) #t
                (has-binding?/ribs saved-env search-var)
                )))))
(define in-varlist?
  (lambda (var search-var)
    (if (not (memv search-var var)) #f
        #t)))

(define extend-env*/ribs
  (lambda (var val env)
    (cons (list var val) env)
    ))


(define empty-env/proc
  (lambda ()
    (list
     (lambda (search-var)
      (report-no-binding-found search-var)
      )
     (lambda () #t)
     (lambda (search-var) #f)
      
    )))

(define extend-env/proc
  (lambda (saved-var saved-val saved-env)
    (list
     (lambda (search-var)
      (if (eqv? search-var saved-var)
          saved-val
          (apply-env/proc saved-env search-var)
          ))
     (lambda () #f)
     (lambda (search-var)
       (or
        (eqv? search-var saved-var)
        (has-binding?/proc saved-env search-var)))
     )))
(define apply-env/proc
  (lambda (env search-var)
    ((car env) search-var)
    ))

(define empty-env?/proc
  (lambda (env)
    ((cadr env))
    ))
(define has-binding?/proc
  (lambda (env search-var)
    ((caddr env) search-var)
    ))


;(define g-ribs (extend-env/ribs 'a 1 (extend-env/ribs 'b 2 (empty-env/ribs))))
;(define h-ribs (extend-env*/ribs '(c d) '(1 2) g-ribs))
;(apply-env/ribs h-ribs 'a)
;(define g (extend-env/proc 'a 1 (extend-env/proc 'b 2 (empty-env/proc))))
;(apply-env/proc g 'a)
;(has-binding?/proc g 'b)