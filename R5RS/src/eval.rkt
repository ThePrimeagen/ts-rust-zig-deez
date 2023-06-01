(load "../src/utils.rkt")
(loader "ast")
(loader "object")
(loader "token")


(define (eval-statements stmts)
  (define result '())
  (for-each (lambda (stmt) (set! result (monkey-eval stmt))) stmts)
  result)

(define (eval-bang-operator-expression right)
  (cond
    ((eq? right THE_TRUE) THE_FALSE)
    ((eq? right THE_FALSE) THE_TRUE)
    ((obj-null? right) THE_TRUE)
    (else THE_FALSE)))

(define (eval-minus-prefix-operator-expression right)
  (if (obj-int? right) (new-int (- (obj-value right))) THE_NULL))

(define (eval-prefix-expression operator right)
  (cond
    ((char-eq? operator "!") (eval-bang-operator-expression right))
    ((char-eq? operator "-") (eval-minus-prefix-operator-expression right))
    
    (else THE_NULL) ))

(define (monkey-eval node)
  (cond
    ((is-parser? node) (eval-statements (parser-stmts node)))
    ((expression-stmt? node) (monkey-eval (expression-value node)))
    ((int-literal? node) (new-int (int-value node)))
    ((bool-literal? node) (new-bool-obj-from-native (bool-value node)))
    ((prefix-exp? node) (let* ((right (monkey-eval (prefix-exp-right node)))) (eval-prefix-expression (prefix-exp-operator node) right)))

    (else THE_NULL)))