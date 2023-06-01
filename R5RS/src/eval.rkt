(load "../src/utils.rkt")
(loader "ast")
(loader "object")
(loader "token")


(define (is-error? obj)
  (if (obj-null? obj) #f (obj-error? obj)))

(define (eval-program stmts)
  (let* ((stmt (car stmts)) (result (monkey-eval stmt)))
    (cond
      ((obj-return-value? result) (obj-value result))
      ((obj-error? result) result)
      ((> (length (cdr stmts)) 0) (eval-program (cdr stmts)))
      (else result))))

(define (eval-block-statement block)
  (define (inner stmts)
    (let* ((stmt (car stmts)) (result (monkey-eval stmt)))
      (cond
        ((and (not (obj-null? result)) (or (obj-return-value? result) (obj-error? result))) result)
        ((> (length (cdr stmts)) 0) (inner (cdr stmts)))
        (else result))))
  (inner (block-stmts block)))

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
  (if (obj-int? right) (new-int (- (obj-value right))) (format-error "unknown operator: " "-" (obj-type right))))

(define (eval-prefix-expression operator right)
  (cond
    ((char-eq? operator "!") (eval-bang-operator-expression right))
    ((char-eq? operator "-") (eval-minus-prefix-operator-expression right))
    
    (else (format-error "unknown operator: " operator (obj-type right)))))

(define (eval-if-expression node)
  (define condition (monkey-eval (if-cond node)))
  (cond
    ((is-error? condition) condition)
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
    ((char-eq? operator "==") (new-bool-obj-from-native (= left-value right-value)))
    ((char-eq? operator "!=") (new-bool-obj-from-native (not (= left-value right-value))))

    (else (format-error "unknown operator: " (obj-type left) " " operator " " (obj-type right)))))

(define (eval-infix-expression operator left right)
  (cond
    ((and (obj-int? left) (obj-int? right)) (eval-integer-infix-expression operator left right))
    ((char-eq? operator "==") (new-bool-obj-from-native (eq? left right)))
    ((char-eq? operator "!=") (new-bool-obj-from-native (not (eq? left right))))

    ((not (eq? (obj-type left) (obj-type right))) (format-error "type mismatch: " (obj-type left) " " operator " " (obj-type right)))
    (else (format-error "unknown operator: " (obj-type left) " " operator " " (obj-type right)))))

(define (monkey-eval node)
  (cond
    ((is-parser? node) (eval-program (parser-stmts node)))
    ((expression-stmt? node) (monkey-eval (expression-value node)))
    ((int-literal? node) (new-int (int-value node)))
    ((bool-literal? node) (new-bool-obj-from-native (bool-value node)))
    ((prefix-exp? node) (let* ((right (monkey-eval (prefix-exp-right node)))) (if (is-error? right) right (eval-prefix-expression (prefix-exp-operator node) right))))
    ((infix-exp? node) (let* ((left (monkey-eval (infix-exp-left node)))) (if (is-error? left) left (let* ((right (monkey-eval (infix-exp-right node)))) (eval-infix-expression (infix-exp-operator node) left right)))))
    ((block-stmt? node) (eval-block-statement node))
    ((if-exp? node) (eval-if-expression node))
    ((return-stmt? node) (let* ((val (monkey-eval (return-value node)))) (if (is-error? val) val (new-return-value val))))

    (else THE_NULL)))