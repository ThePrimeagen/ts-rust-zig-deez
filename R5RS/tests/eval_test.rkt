(load "../src/utils.rkt")
(loader "lexer")
(loader "ast")
(loader "parser")
(loader "eval")
(loader "object")
(loader "environment")

(define (test-eval input)
  (let* ((lexer (new-lexer input))
         (parser (new-parser lexer))
         (p (parse-programme parser))
         (env (new-environment)))
    (monkey-eval p env)))

(define (test-integer-obj evaulated expected)
  (if (not (obj-int? evaulated)) (error (format "Object is not an interger, but was: " (obj-type evaulated))))
  (if (not (= (obj-value evaulated) expected)) (error (format "Object had wrong value, got:" (obj-value evaulated) ", but want:" expected))))

(define (test-boolean-obj obj expected)
  (if (not (obj-bool? obj)) (error (format "Object is not an bool, but was: " (obj-type obj))))
  (if (not (eq? (obj-value obj) expected)) (error (format "Object had wrong value, got:" (obj-value obj) ", but want:" expected))))

(define (test-null-obj obj)
  (if (not (obj-null? obj)) (error (format "Object is not an NULL, but was: " (obj-type obj)))))


(define (test-eval-integer)
  (define tests (list
                 (list "5" 5)
                 (list "10" 10)
                 (list "-5" -5)
                 (list "-10" -10)
                 (list "5 + 5 + 5 + 5 - 10" 10)
                 (list "2 * 2 * 2 * 2 * 2" 32)
                 (list "-50 + 100 + -50" 0)
                 (list "5 * 2 + 10" 20)
                 (list "5 + 2 * 10" 25)
                 (list "20 + 2 * -10" 0)
                 (list "50 / 2 * 2 + 10" 60)
                 (list "2 * (5 + 10)" 30)
                 (list "3 * 3 * 3 + 10" 37)
                 (list "3 * (3 * 3) + 10" 37)
                 (list "(5 + 10 * 2 + 15 / 3) * 2 + -10" 50)))
  (for-each (lambda (t) (define evaluated (test-eval (car t))) (test-integer-obj evaluated (cadr t))) tests))

(define (test-eval-bool)
  (define tests (list
                 (list "true" #t)
                 (list "false" #f)
                 (list "1 < 2" #t)
                 (list "1 > 2" #f)
                 (list "1 < 1" #f)
                 (list "1 > 1" #f)
                 (list "1 == 1" #t)
                 (list "1 != 1" #f)
                 (list "1 == 2" #f)
                 (list "1 != 2" #t)
                 (list "true == true" #t)
                 (list "false == false" #t)
                 (list "true == false" #f)
                 (list "true != false" #t)
                 (list "false != true" #t)
                 (list "(1 < 2) == true" #t)
                 (list "(1 < 2) == false" #f)
                 (list "(1 > 2) == true" #f)
                 (list "(1 > 2) == false" #t)))
  (for-each (lambda (t) (define evaluated (test-eval (car t))) (test-boolean-obj evaluated (cadr t))) tests))

(define (test-bang-operator)
  (define tests (list (list "!true" #f) (list "!false" #t) (list "!5" #f) (list "!!true" #t) (list "!!false" #f) (list "!!5" #t)))
  (for-each (lambda (t) (define evaluated (test-eval (car t))) (test-boolean-obj evaluated (cadr t))) tests))

(define (test-if-else-expressions)
  (define tests (list
                 (list "if (true) { 10 }" 10)
                 (list "if (false) { 10 }" '())
                 (list "if (1) { 10 }" 10)
                 (list "if (1 < 2) { 10 }" 10)
                 (list "if (1 > 2) { 10 }" '())
                 (list "if (1 > 2) { 10 } else { 20 }" 20)
                 (list "if (1 < 2) { 10 } else { 20 }" 10)))
  (for-each (lambda (t) (define evaluated (test-eval (car t))) (if (number? (cadr t)) (test-integer-obj evaluated (cadr t)) (test-null-obj evaluated))) tests))

(define (test-return-statements)
  (define tests (list
                 (list "return 10;" 10)
                 (list "return 10; 9" 10)
                 (list "return 2 * 5; 9" 10)
                 (list "9; return 2 * 5; 9;" 10)
                 (list "if (10 > 1) { if (10 > 1) { return 10; } return 1; }" 10)))
  (for-each (lambda (t) (define evaluated (test-eval (car t))) (test-integer-obj evaluated (cadr t))) tests))

(define (test-error-handling)
  (define tests (list
                 (list "5 + true;" "type mismatch: INTEGER + BOOLEAN")
                 (list "5 + true; 5;" "type mismatch: INTEGER + BOOLEAN")
                 (list "-true" "unknown operator: -BOOLEAN")
                 (list "true + false" "unknown operator: BOOLEAN + BOOLEAN")
                 (list "5; true + false; 5" "unknown operator: BOOLEAN + BOOLEAN")
                 (list "if (10 > 1) { true + false; }" "unknown operator: BOOLEAN + BOOLEAN")
                 (list "foobar" "identifier not found: foobar")
                 (list "\"hello\" - \"world\"" "unknown operator: STRING - STRING")
                 (list "{\"name\": \"Monkey\"}[fn(x) { x }];" "unusable as hash key: FUNCTION")))
  (for-each (lambda (t)
              (define evaluated (test-eval (car t)))             
              (if (not (obj-error? evaluated)) (error (format "no error object returned, got: " evaluated)))
              (if (not (string=? (obj-value evaluated) (cadr t))) (error (format "wrong error message, expected='" (cadr t) "' but got='" (obj-value evaluated) "'")))
              ) tests))

(define (test-let-statements)
  (define tests (list (list "let a = 5; a;" 5) (list "let a = 5 * 5; a;" 25) (list "let a = 5; let b = a; b;" 5) (list "let a = 5; let b = a; let c = a + b + 5; c;" 15)))
  (for-each (lambda (t) (define evaluated (test-eval (car t))) (test-integer-obj evaluated (cadr t))) tests))

(define (test-function-object)
  (define evaluated (test-eval "fn(x) { x + 2; };"))

  (if (not (obj-fn? evaluated))
      (error (format "object is not a function, got" evaluated)))

  (if (not (= (length (obj-fn-params evaluated)) 1))
      (error (format "function has wrong parameters. Parameters= " (car (obj-fn-params evaluated))))))

(define (test-function-applications)
  (define tests (list
                 (list "let identity = fn(x) { x; }; identity(5)" 5)
                 (list "let identity = fn(x) { return x; }; identity(5);" 5)
                 (list "let double = fn(x) { x * 2; }; double(5);" 10)
                 (list "let add = fn(x, y) { x + y; }; add(5, 5);" 10)
                 (list "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));" 20)
                 (list "fn(x) { x; }(5)" 5)))
  (for-each (lambda (t) (define evaluated (test-eval (car t))) (test-integer-obj evaluated (cadr t))) tests))

(define (test-eval-string)
  (define evaluated (test-eval "\"I'm Amelia <3\""))

  (if (not (obj-string? evaluated)) (error (format "object is not String. got:" evaluated)))
  (if (not (string=? (obj-value evaluated) "I'm Amelia <3")) (error (format "String has wrong value. got:" evaluted))))

(define (test-string-concatenation)
  (define evaluated (test-eval "\"I'm \" +  \"Amelia \" +  \"<3\""))

  (if (not (obj-string? evaluated)) (error (format "object is not String. got:" evaluated)))
  (if (not (string=? (obj-value evaluated) "I'm Amelia <3")) (error (format "String has wrong value. got:" evaluated))))

(define (test-buildin-functions)
  (define tests (list
                 (list "len(\"\")" 0)
                 (list "len(\"four\")" 4)
                 (list "len(1)" "argument to 'len' not supported, got INTEGER")
                 (list "len(\"one\",\"two\")" "wrong number of arguments. got=2, want=1")))
  (for-each (lambda (t)
              (define evaluated (test-eval (car t)))
              (cond
                ((number? (cadr t)) (test-integer-obj evaluated (cadr t)))
                ((string? (cadr t)) (begin
                                      (if (not (obj-error? evaluated)) (error (format "no error object returned, got: " evaluated)))
                                      (if (not (string=? (obj-value evaluated) (cadr t)))
                                          (error (format "wrong error message, expected='" (cadr t) "' but got='" (obj-value evaluated) "'"))))))) tests))

(define (test-array-literals)
  (define evaluated (test-eval "[1, 2 * 2, 3 + 3]"))
  (if (not (obj-array? evaluated))
      (error (format "Object is not an array. Got:" evaluated)))
  (define el (obj-value evaluated))
  (if (not (= (length el) 3))
      (error (format "Array does not have 3 elements. Has:" (length el))))

  (test-integer-obj (get-nth-element el 0) 1)
  (test-integer-obj (get-nth-element el 1) 4)
  (test-integer-obj (get-nth-element el 2) 6))

(define (test-array-index-expressions)
  (define tests (list
                 (list "[1, 2, 3][0]" 1)
                 (list "[1, 2, 3][1]" 2)
                 (list "[1, 2, 3][2]" 3)
                 (list "let i = 0;[1][0]" 1)
                 (list "[1, 2, 3][1+1]" 3)
                 (list "let myArray = [1, 2, 3]; myArray[2];" 3)
                 (list "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];" 6)
                 (list "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]" 2)
                 (list "[1, 2, 3][3]" '())
                 (list "[1, 2, 3][-1]" '())))
  (for-each (lambda (t) (define evaluated (test-eval (car t))) (if (number? (cadr t)) (test-integer-obj evaluated (cadr t)) (test-null-obj evaluated))) tests))

(define (test-hash-literal)
  (define evaluated (test-eval "let two = \"two\";
{
           \"one\": 10 - 9,
           two: 1 + 1,
           \"thr\" + \"ee\": 6 / 2,
           4: 4,
           true: 5,
           false: 6
}"))

  (if (not (obj-hash? evaluated))
      (error (format "Eval didn't return Hash. Got:" evaluated)))

  (define expected (list
                    (list (make-hash-key (new-string-obj "one")) 1)
                    (list (make-hash-key (new-string-obj "two")) 2)
                    (list (make-hash-key (new-string-obj "three")) 3)
                    (list (make-hash-key (new-int 4)) 4)
                    (list (make-hash-key THE_TRUE) 5)
                    (list (make-hash-key THE_FALSE) 6)))

  (if (not (= (length expected) (hash-count (hash-obj-hash evaluated))))
      (error (format "Hash has wrong num of pairs. Got:" (hash-count (hash-obj-hash evaluated)))))

  (for-each (lambda (exp)
              (define hash-pair (hash-obj-ref evaluated (car exp)))
              (if (null? hash-pair)
                  (error (format "No hash-pair for the given key:" (car exp))))
              (test-integer-obj (hash-pair-value hash-pair) (cadr exp))) expected))

(define (test-hash-index-exp)
  (define tests (list
                 (list "{\"foo\": 5}[\"foo\"]" 5)
                 (list "{\"foo\": 5}[\"bar\"]" '())
                 (list "let key = \"foo\"; {\"foo\": 5}[key]" 5)
                 (list "{}[\"foo\"]" '())
                 (list "{5: 5}[5]" 5)
                 (list "{true: 5}[true]" 5)
                 (list "{false: 5}[false]" 5)))
  (for-each (lambda (t) (define evaluated (test-eval (car t))) (if (number? (cadr t)) (test-integer-obj evaluated (cadr t)) (test-null-obj evaluated))) tests))
  

(display-nl "Starting eval tests...")
(test-eval-integer)
(test-eval-bool)
(test-bang-operator)
(test-if-else-expressions)
(test-return-statements)
(test-error-handling)
(test-let-statements)
(test-function-object)
(test-function-applications)
(test-eval-string)
(test-string-concatenation)
(test-buildin-functions)
(test-array-literals)
(test-array-index-expressions)
(test-hash-literal)
(test-hash-index-exp)
(display-nl "\tEval tests have passed without errros")