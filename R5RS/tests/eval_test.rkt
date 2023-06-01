(load "../src/utils.rkt")
(loader "lexer")
(loader "ast")
(loader "parser")
(loader "eval")
(loader "object")

(define (test-eval input)
  (let* ((lexer (new-lexer input))
         (parser (new-parser lexer))
         (p (parse-programme parser)))
    (monkey-eval p)))

(define (test-integer-obj evaulated expected)
  (if (not (obj-int? evaulated)) (error (format "Object is not an interger, but was: " (obj-type evaulated))))
  (if (not (= (obj-value evaulated) expected)) (error (format "Object had wrong value, got:" (obj-value evaulated) ", but want:" expected))))

(define (test-boolean-obj obj expected)
  (if (not (obj-bool? obj)) (error (format "Object is not an bool, but was: " (obj-type obj))))
  (if (not (eq? (obj-value obj) expected)) (error (format "Object had wrong value, got:" (obj-value obj) ", but want:" expected))))


(define (test-eval-integer)
  (define tests (list (list "5" 5) (list "10" 10) (list "-5" -5) (list "-10" -10)))
  (for-each (lambda (t) (define evaluated (test-eval (car t))) (test-integer-obj evaluated (cadr t))) tests))

(define (test-eval-bool)
  (define tests (list (list "true" #t) (list "false" #f)))
  (for-each (lambda (t) (define evaluated (test-eval (car t))) (test-boolean-obj evaluated (cadr t))) tests))

(define (test-bang-operator)
  (define tests (list (list "!true" #f) (list "!false" #t) (list "!5" #f) (list "!!true" #t) (list "!!false" #f) (list "!!5" #t)))
  (for-each (lambda (t) (define evaluated (test-eval (car t))) (test-boolean-obj evaluated (cadr t))) tests))

(test-eval-integer)
(test-eval-bool)
(test-bang-operator)