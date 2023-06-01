(load "../src/utils.rkt")
(loader "ast")
(loader "object")
(loader "token")


(define (eval-program stmts)
  (define result '())
  (define return-val '())
  (for-each (lambda (stmt) (set! result (monkey-eval stmt)) (if (obj-return-value? result) (set! return-val result))) stmts) ;; TODO REWRITE WITH RECURSION TO STOP DOING ALL OF THEM
  (if (null? return-val) result (obj-value return-val)))

(define (eval-block-statement block)
  (define result '())
  (define return-val '())
  ;; (null? return-val) differce from the book as we continue to loop over. See TODO for fix
  (for-each (lambda (stmt) (set! result (monkey-eval stmt)) (if (and (obj-return-value? result) (not (null? result)) (null? return-val)) (set! return-val result))) (block-stmts block)) ;; TODO REWRITE WITH RECURSION TO STOP DOING ALL OF THEM
  (if (null? return-val) result return-val))

(define (eval-bang-operator-expression right)
  (cond
    ((eq? right THE_TRUE) THE_FALSE)
    ((eq? right THE_FALSE) THE_TRUE)
    ((obj-null? right) THE_TRUE)
    (else THE_FALSE)))

(define (is-truthy obj)
  (cond
    ((obj-null? obj) #f)
    ((eq? obj THE_TRUE) #t)
    ((eq? obj THE_FALSE) #f)
    (else #t)))

(define (eval-minus-prefix-operator-expression right)
  (if (obj-int? right) (new-int (- (obj-value right))) THE_NULL))

(define (eval-prefix-expression operator right)
  (cond
    ((char-eq? operator "!") (eval-bang-operator-expression right))
    ((char-eq? operator "-") (eval-minus-prefix-operator-expression right))
    
    (else THE_NULL)))

(define (eval-if-expression node)
  (define condition (monkey-eval (if-cond node)))
  (cond
    ((is-truthy condition) (monkey-eval (if-cons node)))
    ((not (obj-null? (if-alt node))) (monkey-eval (if-alt node)))
    (else THE_NULL)))

(define (eval-integer-infix-expression operator left right)
  (define left-value (obj-value left))
  (define right-value (obj-value right))
  (cond
    ((char-eq? operator "+") (new-int (+ left-value right-value)))
    ((char-eq? operator "-") (new-int (- left-value right-value)))
    ((char-eq? operator "/") (new-int (/ left-value right-value)))
    ((char-eq? operator "*") (new-int (* left-value right-value)))
    ((char-eq? operator "<") (new-bool-obj-from-native (< left-value right-value)))
    ((char-eq? operator ">") (new-bool-obj-from-native (> left-value right-value)))
    ((string=? operator "==") (new-bool-obj-from-native (= left-value right-value)))
    ((string=? operator "!=") (new-bool-obj-from-native (not (= left-value right-value))))

    (else THE_NULL)))

(define (eval-infix-expression operator left right)
  (cond
    ((and (obj-int? left) (obj-int? right)) (eval-integer-infix-expression operator left right))
    ((string=? operator "==") (new-bool-obj-from-native (eq? left right)))
    ((string=? operator "!=") (new-bool-obj-from-native (not (eq? left right))))
    
    (else THE_NULL)))

(define (monkey-eval node)
  (cond
    ((is-parser? node) (eval-program (parser-stmts node)))
    ((expression-stmt? node) (monkey-eval (expression-value node)))
    ((int-literal? node) (new-int (int-value node)))
    ((bool-literal? node) (new-bool-obj-from-native (bool-value node)))
    ((prefix-exp? node) (let* ((right (monkey-eval (prefix-exp-right node)))) (eval-prefix-expression (prefix-exp-operator node) right)))
    ((infix-exp? node) (let* ((left (monkey-eval (infix-exp-left node))) (right (monkey-eval (infix-exp-right node)))) (eval-infix-expression (infix-exp-operator node) left right)))
    ((block-stmt? node) (eval-block-statement node))
    ((if-exp? node) (eval-if-expression node))
    ((return-stmt? node) (let* ((val (monkey-eval (return-value node)))) (new-return-value val)))

    (else THE_NULL)))