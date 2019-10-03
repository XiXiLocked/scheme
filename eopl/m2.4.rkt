#lang eopl
(define identifier?
  (lambda (name)
    (and (symbol? name)
         (not (eqv? name 'lambda))
         )))

(define-datatype lc-exp lc-exp?
  (var-exp
   (var identifier?))
  (lambda-exp
   (bound-var identifier?)
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rand lc-exp?)))
; comment for define-datetype form :
;lc-exp is used in 'cases form
;identifier? lc-exp? are predictives
;var-exp, lambda-exp, app-exp are constructors
; var, bound-var, body, rator, rand are placeholders for constructor, not mentioned after definition



(define occurs-free?
  (lambda (search-var exp)
    (cases lc-exp exp
      (var-exp (var) (eqv? var search-var))
      (lambda-exp (bound-var body)
                  (and
                   (not (eqv? search-var bound-var))
                   (occurs-free? search-var body)))
      (app-exp (rator rand)
               (or
                (occurs-free? search-var rator)
                (occurs-free? search-var rand))))))

; comments for 'cases form
; lc-exp, var-exp, app-exp  are defined in define-datatype, and used as tags in cases
; exp, var, bound-var, body, rator, rand are bound variables for consequent. The names are not relavent to define-datatype



(define-datatype bintree bintree?
  (leaf-node
   (num integer?))
  (interior-node
   (key symbol?)
   (left bintree?)
   (right bintree?))
  )

(define bintree-to-list
  (lambda (bintree_exp)
    (cases bintree bintree_exp
      (leaf-node (num) (list 'leaf-node num))
      (interior-node (key left right)
                     (list 'interior-node
                           key
                           (bintree-to-list left)
                           (bintree-to-list right))
                     )
      )))


(define max-interior
  (lambda (bintree_exp)
    ;sum-max return (leafsum of tree, (maxsum of tree, key of corresponding tree))
    ; car caadr cadadr)
    (letrec ((sum-max (lambda (bintree_exp)
                        (cases bintree bintree_exp
                          (leaf-node (num) (list num))
                          (interior-node (key left right)
                                         (let* ((left-result (sum-max left))
                                                (right-result (sum-max right))
                                                (tree-sum (+ (car left-result) (car right-result)))
                                                (sum-max_lr (if (null? (cdr left-result))
                                                                right-result
                                                                (if (null? (cdr right-result))
                                                                    left-result
                                                                    (if (> (caadr left-result) (caadr right-result))
                                                                        left-result
                                                                        right-result)))))
                                           (if (null? (cdr sum-max_lr))
                                               (list tree-sum (list tree-sum key))
                                               (if (> (caadr sum-max_lr) tree-sum)
                                                   (cons tree-sum (cdr sum-max_lr))
                                                   (list tree-sum (list tree-sum key)))
                                               )))))))
      (cadadr (sum-max bintree_exp))
      )))

(define-datatype Red-blue-tree Red-blue-tree?
  (Red-blue-subtree-exp (rb-subtree Red-blue-subtree?)
                        ))
(define-datatype Red-blue-subtree Red-blue-subtree?
  (red-node-exp (nodetype (lambda (nodetype) (eqv? nodetype 'red-node)))
                (Red-blue-subtree1 Red-blue-subtree?)
                (Red-blue-subtree2 Red-blue-subtree?))
  (blue-node-exp (nodetype (lambda (nodetype) (eqv? nodetype 'blue-node)))
                 (rest list-of-Red-blue-subtree?))
  (leaf-node-exp (nodetype (lambda (nodetype) (eqv? nodetype 'leaf-node)))
                 (num integer?))
  )
(define-datatype list-of-Red-blue-subtree list-of-Red-blue-subtree?
  (empty-list)
  (non-empty-list (first Red-blue-subtree?)
                  (rest list-of-Red-blue-subtree?)
                  ))

(define count-red-node
  (lambda (tree)
    (cases Red-blue-tree tree
      (Red-blue-subtree-exp
       (rbtree)
       (letrec ((count-red-forlist
                (lambda (a-tree red-count)
                  (cases list-of-Red-blue-subtree a-tree
                    (empty-list () a-tree)
                    (non-empty-list (first rest) (non-empty-list (count-red first red-count) (count-red-forlist rest red-count)))
                    )))
                (count-red
                 (lambda (a-tree red-count)
                   (cases Red-blue-subtree a-tree
                     (red-node-exp (nodetype tree1 tree2)
                                   (red-node-exp nodetype (count-red tree1 (+ red-count 1)) (count-red tree2 (+ red-count 1))))
                     (blue-node-exp (nodetype rest)
                                    (blue-node-exp nodetype (count-red-forlist rest red-count)))
                     (leaf-node-exp (nodetype num)
                                    (leaf-node-exp nodetype red-count))
                     ))))
         (count-red rbtree 0))
       ))))


;exercise 2.24
(bintree-to-list (interior-node 'a (leaf-node 3) (leaf-node 4)))
;exercise 2.25
(define tree-1
  (interior-node 'foo (leaf-node 2) (leaf-node 3)))
(define tree-2
  (interior-node 'bar (leaf-node -1) tree-1))
(define tree-3
  (interior-node 'baz tree-2 (leaf-node 1)))
(max-interior tree-2)
(max-interior tree-3)

;exercise 2.26
(define t1 (Red-blue-subtree-exp
            (red-node-exp 'red-node
                          (leaf-node-exp 'leaf-node 5)
                          (leaf-node-exp 'leaf-node 6) )))
(count-red-node t1)
(define t2 (Red-blue-subtree-exp
            (blue-node-exp 'blue-node
                           (non-empty-list
                            (red-node-exp 'red-node
                                          (leaf-node-exp 'leaf-node 5)
                                          (leaf-node-exp 'leaf-node 6))
                            (empty-list)))))
(count-red-node t2)

(define t3 (Red-blue-subtree-exp
            (red-node-exp 'red-node
            (blue-node-exp 'blue-node
                           (non-empty-list
                            (red-node-exp 'red-node
                                          (leaf-node-exp 'leaf-node 5)
                                          (leaf-node-exp 'leaf-node 6))
                            (empty-list)))
            (leaf-node-exp 'leaf-node 7))))
(count-red-node t3)
