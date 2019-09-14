#lang racket
;1.15
(define duple
  (lambda (n x)
    (if (zero? n)
        '()
        (cons x (duple (- n 1) x))
        )))
;1.16
(define (invert lst)
  (let* ((inv (lambda (twolist) (list (cadr twolist) (car twolist))))
         (invert (lambda (lst)
                   (if (null? lst) '()
                       (cons (inv (car lst)) (invert (cdr lst)))))))
    (invert lst)))
;1.17
(define (down lst)
  (if (null? lst) '()
      (cons (cons (car lst) '()) (down (cdr lst)))))
;1.18
(define (swapper s1 s2 slist)
  (letrec ((sel (lambda (a)
                  (cond
                    ((eqv? s1 a) s2)
                    ((eqv? s2 a) s1)
                    (else a))))
           (swapper (lambda (slist)
                      (if (null? slist) '()
                          (let ((s (car slist)))
                            (if (symbol? s) (cons (sel s) (swapper (cdr slist)))
                                (cons (swapper s) (swapper (cdr slist)))))))))
    (swapper slist)))
;1.19
(define (list-set lst n x)
  (if (null? lst) '()
      (if (zero? n)
          (cons x lst)
          (cons (car lst) (list-set  (cdr lst) (- n 1) x))
          )))
;1.20
(define (count-occurrences s slist)
  (letrec ((symbol-occurences
            (lambda (sym) (if (eqv? sym s) 1 0)))
           (sexp-occurences
            (lambda (sexp)
              (if (symbol? sexp) (symbol-occurences sexp)
                  (slist-occurences sexp))))
           (slist-occurences
            (lambda (slist)
              (if (null? slist) 0
                  (+ (sexp-occurences (car slist)) (slist-occurences (cdr slist)))))))
    (slist-occurences slist)))
;1.21
(define (product sos1 sos2)
  (letrec ((xprod (lambda (x sos2 acc)
                    (if (null? sos2) acc
                        (cons (list x  (car sos2) ) (xprod x (cdr sos2)acc)))))
           (prod (lambda (sos1 acc)
                   (if (null? sos1) acc
                       (xprod (car sos1) sos2 (prod (cdr sos1) acc)))))
           )
    (prod sos1 '())))
;1.22
(define (filter-in* pred lst)
  (letrec ((filter-sexp (lambda (sexp cc)
                          (if (list? sexp)
                              (filter-slist sexp cc )
                              (if (pred sexp)
                                  (lambda (l) (cc (cons sexp l)))
                                  cc))))
           (filter-slist (lambda (slist cc)
                           (if (null? slist)
                               cc
                               (filter-slist (cdr slist)
                                             (filter-sexp (car slist)
                                                          cc )))))
           )
    ((filter-slist lst (lambda (e) e)) '())
    ))
;1.23
(define (filter-in pred lst)
  (letrec ((filter-sexp (lambda (sexp cc)
                          (if (pred sexp)
                              (lambda (l) (cc (cons sexp l)))
                              cc)))
           (filter-slist (lambda (slist cc)
                           (if (null? slist)
                               cc
                               (filter-slist (cdr slist)
                                             (filter-sexp (car slist)
                                                          cc )))))
           )
    ((filter-slist lst (lambda (e) e)) '())
    ))
;1.24
(define (list-index pred lst)
  (letrec ((list-index (lambda (lst index)
                         (if (null? lst) #f
                             (let ((v (car lst)))
                               (if (pred v)
                                   index
                                   (list-index (cdr lst) (+ index 1))
                                   ))))))
    (list-index lst 0)))
;1.25
(define (every? pred lst)
  (letrec ((every? (lambda (lst)
                     (if (null? lst) #t
                         (and
                          (pred (car lst))
                          (every? (cdr lst)))))
                   ))
    (every? lst)))
(define (exists? pred lst)
  (letrec ((exists? (lambda (lst)
                      (if (null? lst) #f
                          (or
                           (pred (car lst))
                           (exists? (cdr lst)))))
                    ))
    (exists? lst)))
;1.26
(define (up lst)
  (letrec ((prepend (lambda (pre lst)
                      (if (null? pre)
                          lst
                          (cons (car pre) (prepend (cdr pre) lst))))
                    )
           (up (lambda (lst)
                 (if (null? lst)
                     '()
                     (let ((head (car lst)))
                       (if (list? head)
                           (prepend head (up (cdr lst)))
                           (cons head (up (cdr lst)))))
                     ))))
    (up lst)))
;1.27
(define (flatten slist)
  (letrec ((flatten (lambda (slist collected)
                      (if (null? slist)
                          collected
                          (let ((head (car slist)))
                            (if (symbol? head)
                                (cons head (flatten (cdr slist) collected))
                                (flatten head (flatten (cdr slist) collected))))))
                    ))
    (flatten slist '())
    ))
;1.28
(define (merge loi1 loi2)
  (if (null? loi1) loi2
      (if (null? loi2) loi1
          (let ((i1 (car loi1))
                (i2 (car loi2)))
            (if (< i1 i2)
                (cons i1 (merge (cdr loi1) loi2))
                (cons i2 (merge loi1 (cdr loi2))))
            ))))
;1.29
(define (sort loi)
  (if (null? loi) '()
      (let* ((pivot (car loi))
             (loi1 (filter-in (lambda (i) (>= i pivot)) (cdr loi)))
             (loi2 (filter-in (lambda (i) (< i pivot))  (cdr loi))))
        (merge (cons pivot (sort loi1)) (sort loi2))
        )))
;1.30
(define (sort/predicate pred loi)
  (letrec ((merge (lambda (loi1 loi2)
                    (if (null? loi1) loi2
                        (if (null? loi2) loi1
                            (let ((i1 (car loi1))
                                  (i2 (car loi2)))
                              (if (pred i1 i2)
                                  (cons i1 (merge (cdr loi1) loi2))
                                  (cons i2 (merge loi1 (cdr loi2))))
                              )))))
           (sort (lambda (loi)
                   (if (null? loi) '()
                       (let* ((pivot (car loi))
                              (loi1 (filter-in (lambda (i) (not (pred i pivot))) (cdr loi)))
                              (loi2 (filter-in (lambda (i) (pred i pivot))  (cdr loi))))
                         (merge (cons pivot (sort loi1)) (sort loi2))
                         )))))
    (sort loi)
    ))
;1.31
(define (leaf i)
  i)

(define (interior-node s B C)
  (list s B C))

(define (leaf? n)
  (number? n))

(define (lson n)
  (cadr n))

(define (rson n)
  (caddr n))

(define (contents-of n)
  (car n))
;1.32
(define (double-tree n)
  (if (leaf? n)
      (leaf (* n 2))
      (interior-node
       (contents-of n)
       (double-tree (lson n))
       (double-tree (rson n)))
      ))
;1.33
(define (mark-leaves-with-red-depth n)
  (letrec ((mark-leaves-with-red-depth
            (lambda (n red-count)
              (if (leaf? n)
                  (leaf red-count)
                  (let* ((sym (contents-of n))
                         (new-red-count (if (eqv? sym 'red)
                                            (+ red-count 1)
                                            red-count)))
                    (interior-node
                     sym
                     (mark-leaves-with-red-depth (lson n) new-red-count)
                     (mark-leaves-with-red-depth (rson n) new-red-count)))
                  ))))
    (mark-leaves-with-red-depth n 0)))
;1.34
(define (path n bst)
  (if (null? bst) '() ;;error
      (let ((v (car bst)))
        (if (eqv? v n) '()
            (if (< v n)
                (cons 'right (path n (caddr bst)))
                (cons 'left (path n (cadr bst)))
                )))))
;1.35
(define (number-leaves b-tree)
  (letrec ((number-leaves (lambda (b-tree i)
                            (if (leaf? b-tree)
                                (list (leaf i) (+ i 1))
                                (let* ((result-lson (number-leaves (lson b-tree) i))
                                       (newlson (car result-lson))
                                       (newi    (cadr result-lson))
                                       (result-rson (number-leaves (rson b-tree) newi))
                                       (newrson (car result-rson))
                                       (newii   (cadr result-rson)))
                                  (list
                                   (interior-node
                                    (contents-of b-tree)
                                    newlson
                                    newrson)
                                   newii)
                                  )))))
    (car (number-leaves b-tree 0))
    ))

;1.36
(define number-elements
         (lambda (lst)
           (if (null? lst) '()
               (g (list 0 (car lst)) (number-elements (cdr lst))))))

(define (g lst kst)
  (if (null? kst)
     (cons lst '())
      (cons lst (map
                 (lambda (x) (list (+ 1 (car x)) (cadr x)))
                  kst)))
  )
;;;;;;;;;;;;;;;;;;;;;;;;test
(define (test1.15)
  (display (duple 2 3))
  (display (duple 4 '(ha ha)) )
  (display (duple 0 '(blah)))
  )

(define (test1.16)
  (display (invert '((a 1) (a 2) (1 b) (2 b)))))

(define (test1.17)
  (display (down '(1 2 3)))
  (display (down '((a) (fine) (idea))))
  (display (down '(a (more (complicated)) object))
           ))

(define (test1.18)
  (display (swapper 'a 'd '(a b c d)))
  (display (swapper 'a 'd '(a d () c d)))
  (display (swapper 'x 'y '((x) y (z (x)))))
  )

(define (test1.19)
  (display (list-set '(a b c d) 2 '(1 2)))
  (display (list-ref (list-set '(a b c d) 3 '(1 5 10)) 3))
  )

(define (test1.20)
  (display (count-occurrences 'x '((f x) y (((x z) x)))))
  (display (count-occurrences 'x '((f x) y (((x z) () x)))))
  (display (count-occurrences 'w '((f x) y (((x z) x))))))

(define (test1.21)
  (display (product '(a b c) '(x y))))

(define (test1.22)
  (display (filter-in number? '(a 2 (1 3) b 7)))
  (display (filter-in symbol? '(a (b c) 17 foo)))
  )

(define (test1.23)
  (display (list-index number? '(a 2 (1 3) b 7)))
  (display (list-index symbol? '(a (b c) 17 foo)))
  (display (list-index symbol? '(1 2 (a b) 3)))
  )
(define (test1.24)
  (display (every? number? '(a b c 3 e)))
  (display (every? number? '(1 2 3 5 4)))
  )
(define (test1.25)
  (display (exists? number? '(a b c 3 e)))
  (display (exists? number? '(a b c d e)))
  )
(define (test1.26)
  (display (up '((1 2) (3 4))))
  (display (up '((x (y)) z)))
  )
(define (test1.27)
  (display (flatten '(a b c)))
  (display (flatten '((a) () (b ()) () (c))))
  (display (flatten '((a b) c (((d)) e))))
  (display (flatten '(a b (() (c)))))
  )
(define (test1.28)
  (display (merge '(1 4) '(1 2 8)))
  (display (merge '(35 62 81 90 91) '(3 83 85 90)))
  )
(define (test1.29)
  (display (sort '(8 2 5 2 3)))
  )
(define (test1.30)
  (display (sort/predicate < '(8 2 5 2 3)))
  (display (sort/predicate > '(8 2 5 2 3)))
  )

(define (test1.33)
  (display (mark-leaves-with-red-depth
            (interior-node 'red
                           (interior-node 'bar
                                          (leaf 26)
                                          (leaf 12))
                           (interior-node 'red
                                          (leaf 11)
                                          (interior-node 'quux
                                                         (leaf 117)
                                                         (leaf 14))
                                          ))))
  )
(define (test1.34)
  (display (path 17 '(14 (7 () (12 () ())) (26 (20 (17 () ())
                                                   ())
                                               (31 () ())))))
  )

(define (test1.35)
  (display (number-leaves
    (interior-node 'foo
      (interior-node 'bar
        (leaf 26)
        (leaf 12))
      (interior-node 'baz
        (leaf 11)
        (interior-node 'quux
          (leaf 117)
          (leaf 14))
        ))))
  )
