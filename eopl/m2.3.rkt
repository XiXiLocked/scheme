#lang racket

(define number->sequence
  (lambda (number)
    (list number '() '())
    ))
(define current-element-seq
  (lambda (seq)
    (car seq)
    ))
(define move-to-left-seq
  (lambda (seq)
    (if (at-left-end? seq) #f
    (list (caadr seq) (cdadr seq) (cons (car seq) (caddr seq)))
    )))
(define move-to-right-seq
  (lambda (seq)
    (if (at-right-end? seq) #f
    (list (caaddr seq) (cons (car seq) (cadr seq)) (cdaddr seq))
    )))

(define insert-to-left-seq
  (lambda (num seq)
    (list (car seq) (cons num( cadr seq)) (caddr seq))
    ))

(define insert-to-right-seq
  (lambda (num seq)
    (list (car seq) (cadr seq) (cons num (caddr seq)))
    ))
(define at-left-end?
  (lambda (seq)
    (null? (cadr seq))
    ))
(define at-right-end?
  (lambda (seq)
    (null? (caddr seq))
    ))



(define number->bintree
  (lambda (num)
    (list num '() '())
    ))
(define current-element
  (lambda (bintree)
    (if (null? bintree) #f
        (car bintree)
        )))

(define move-to-left-son
  (lambda (bintree)
    (if (null? bintree) #f
        (cadr bintree)
        )))
(define move-to-right-son
  (lambda (bintree)
    (if (null? bintree) #f
        (caddr bintree)
        )))
(define at-leaf?
  (lambda (bintree)
    (null? bintree)
    ))

(define insert-to-left
  (lambda (num bintree)
    (list (current-element bintree) (list num (move-to-left-son bintree) '()) (move-to-right-son bintree))
        ))

(define insert-to-right
  (lambda (num bintree)
    (list (current-element bintree)  (move-to-left-son bintree) (list num '() (move-to-right-son bintree)))
    ))



; Exercise 2.20
;Tree := (cons parent-path bintree)
; parent-path is a reversed list in wihch  current node is  replaced with placeholder '-
(define number->bintree220
  (lambda (num)
    (cons '() (number->bintree num))
    ))
(define current-element220
  (lambda (bintree)
    (if (at-leaf?220 bintree) #f
        (cadr bintree)
        )))

(define move-to-left-son220
  (lambda (bintree)
    (if (at-leaf?220 bintree) #f
        (let ((parent-path (car bintree))
              (current (current-element220 bintree))
              (left (caddr bintree))
              (right (cadddr bintree)))
          (cons (cons (list current '- right) parent-path) left)
          ))))

(define move-to-right-son220
  (lambda (bintree)
    (if (at-leaf?220 bintree) #f
        (let ((parent-path (car bintree))
              (current (current-element220 bintree))
              (left (caddr bintree))
              (right (cadddr bintree)))
          (cons (cons (list current left '-) parent-path) right)
          ))))

(define at-root?220
  (lambda (bintree)
    (null? (car bintree))
        ))
(define at-leaf?220
  (lambda (bintree)
    (at-leaf? (cdr bintree))
    ))

(define insert-to-left220
  (lambda (num bintree)
    (cons (car bintree) (insert-to-left num (cdr bintree)))
    ))

(define insert-to-right220
  (lambda (num bintree)
    (cons (car bintree) (insert-to-right num (cdr bintree)))
    ))

(define move-up220
  (lambda (bintree)
    (if (at-root?220 bintree) #f
        (let* ((parent-path (car bintree))
               (parent (car parent-path))
               (parent-parent-path (cdr parent-path))
               (parent-val (car parent))
               (parent-left (cadr parent))
               (parent-right (caddr parent))
               (node (cdr bintree)))
          (if (eqv? parent-left '-)
              (cons parent-parent-path (list parent-val node parent-right))
              (cons parent-parent-path (list parent-val parent-left node))
              )))))
              


;(number->sequence 7)
;(current-element-seq '(6 (5 4 3 2 1) (7 8 9)))
;(move-to-left-seq '(6 (5 4 3 2 1) (7 8 9)))
;(move-to-left-seq (number->sequence 7))
;(move-to-right-seq '(6 (5 4 3 2 1) (7 8 9)))
;(move-to-right-seq (number->sequence 7))
;(insert-to-left-seq 13 '(6 (5 4 3 2 1) (7 8 9)))
;(insert-to-right-seq 13 '(6 (5 4 3 2 1) (7 8 9)))



;(number->bintree 13)
;(define t1 (insert-to-right 14
;                            (insert-to-left 12
;                                            (number->bintree 13))))
;t1
;(move-to-left-son t1)
;(current-element (move-to-left-son t1))
;(at-leaf? (move-to-right-son (move-to-left-son t1)))
;(insert-to-left 15 t1)


;(define t (insert-to-right220 2 (insert-to-left220 1 (number->bintree220 0)))
;(move-up220  (move-to-left-son220 t))