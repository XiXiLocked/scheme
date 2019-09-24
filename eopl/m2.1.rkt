#lang racket
;  Diff-tree ::== (one) | (diff Difff-tree Diff-tree
(define (Diff-tree diff-tree1 diff-tree2)
  (list 'diff diff-tree1 diff-tree2)
  )


(define (zero)
  (list 'diff '(one) '(one))
  )
(define (one)
  (list 'one)
  )
(define literally-one?
  (lambda (diff-tree)
    (eqv? (car diff-tree) 'one)
    ))
(define left
  (lambda (diff-tree)
    (cadr diff-tree))
  )
(define right
  (lambda (diff-tree)
    (caddr diff-tree))
  )

(define (is-zero? diff-tree)
  ;(display diff-tree)
  ;(display (to->int diff-tree))
   ;(newline)
  (if (literally-one? diff-tree) #f ;('one)
      (let ((diff-treeA (left diff-tree))
            (diff-treeB (right diff-tree)))
        (if  (literally-one? diff-treeB)
             (if (literally-one? diff-treeA) #t ; ('diff ('one) ('one) )==0 
                 (let ((remains (predecessor- diff-treeA)))
                   (if (eqv? remains #f) #f
                       ; (is-zeros? ('diff T 'one)) == (iszero? (predecessor- T))
                       ;; (diff n+1 -1) == n
                       (is-zero? remains)
                       )))
             ; (is-zero? (diff X Y))== (is-zero? (diff (Y X))
             ;=> (is-zero?  (diff A (diff B C))) == (is-zero?  (diff (diff B A) C))
             ;; A-(B-C) neg-> (B-A)-C
             (let ((A diff-treeA)
                   (B (left diff-treeB))
                   (C (right diff-treeB)))
               (is-zero? (Diff-tree (Diff-tree B A) C)))))
      ))

(define (successor diff-tree)
  (successor+ diff-tree)
  )


(define (successor+ diff-tree)
  ;; +1 by increasing diff-tree nodes
  (Diff-tree diff-tree (Diff-tree (zero) '(one)))
  )
(define (successor- diff-tree)
  ;; +1 by decreasing diff-tree nodes
  ;; it is not always possible, when it fails, it returns #f
  (if (literally-one? diff-tree) #f ; ('one)
      (let ((A (left diff-tree))
            (B (right diff-tree)))
        (if (literally-one? B)
            A  ;succ A-1 = A
            (let ((succA (successor- A)))
              (if (eqv? succA #f)
                  (let ((predB (predecessor- B)))
                    (if (eqv? predB #f) #f 
                        (Diff-tree A predB))) ; n+1 = 
                  (Diff-tree succA B)))
            ))
      ))

(define (predecessor diff-tree)
  (predecessor+ diff-tree)
  )
(define (predecessor+ diff-tree)
  (Diff-tree diff-tree '(one))
  )

(define (predecessor- diff-tree)
  ;; +1 by decreasing diff-tree nodes
  ;; it is not always possible, when it fails, it returns #f
  (if (literally-one? diff-tree) #f ; ('one)
      (let ((A (left diff-tree))
            (B (right diff-tree)))
        (if (and
             (literally-one? A)
             (literally-one? B))
            #f  ;pred 1-1 = succ(0) = -1
            (let ((preA (predecessor- A)))
              (if (eqv? preA #f)
                  (let ((succB (successor- B)))
                    (if (eqv? succB #f) #f 
                        (Diff-tree A succB)))
                  (Diff-tree preA B)))
            ))
      ))
(define (predecessor-X diff-tree)
  ;; +1 by decreasing diff-tree nodes
  ;; it is not always possible, when it fails, it returns #f
  (if (literally-one? diff-tree) #f ; ('one)
      (let ((A (left diff-tree))
            (B (right diff-tree)))
        (if (and
             (literally-one? A)
             (literally-one? B))
             #f  ;pred 1-1 = succ(0) = -1
            (let ((succA (successor- A)))
              (if (eqv? succA #f)
                  (let ((predB (predecessor- B)))
                    (if (eqv? predB #f) #f 
                        (Diff-tree A predB)))
                  (Diff-tree succA B)))
            ))
      ))
  
(define (diff-tree-plus Diff-treeA  Diff-treeB)
  (if (literally-one? Diff-treeB)
       (Diff-tree Diff-treeA (Diff-tree (zero) Diff-treeB))
       (let ((C (cadr Diff-treeB))
             (D (caddr Diff-treeB)))
         ;; A+ (C-D) = A -(D-C)
         (Diff-tree Diff-treeA (Diff-tree D C)))
       ))

(define to->int
  (lambda (diff-tree)
    (if (eqv? diff-tree #f) #f
    (if (literally-one? diff-tree) 1
        (- (to->int (cadr diff-tree)) (to->int (caddr diff-tree)))
        ))))

;
;(display "predecessor" )
;(newline)
;(to->int (predecessor- (predecessor (zero))))
;(to->int (predecessor- (zero)))
;(to->int (predecessor- (one)))
;(to->int (predecessor- (successor (one))))
;(to->int (predecessor- (successor (successor (one)))))
;
;(display "successor" )
;(newline)
;(to->int (successor- (predecessor (zero))))
;(to->int (successor- (zero)))
;(to->int (successor- (one)))
;(to->int (successor- (successor (one))))
;(to->int (successor- (successor (successor (one)))))
;
;(display "is-zero?" )
;(newline)
;(is-zero? (predecessor (zero)))
;(is-zero? (zero))
;(is-zero? (one))
;(is-zero? (successor (one)))
;(is-zero? (successor (successor (one))))
;(is-zero? (Diff-tree (Diff-tree (zero) (zero)) (zero)))
;(is-zero? (Diff-tree (Diff-tree (zero) (one)) (zero)))