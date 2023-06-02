(load "../src/utils.rkt")
(loader "lexer")
(loader "ast")
(loader "parser")


(define (check-parse-errors p)
  (if (not (= 0 (length (parser-errors p))))
      (begin (for-each (lambda (error) (display-l error)) (parser-errors p)) (error (format "Parses had " (length (parser-errors p)) " error(s)!" )))
      )
  )

(define (test-let-stmt stmt name)
  (if (not (let-node? stmt))
      (error (format stmt " was not a let stmt!")))

  (if (not (string=? (identifier-value (let-id stmt)) name))
      (error (format "Wrong name.value, not :'" name "' got:'"  (identifier-value (let-id stmt)) "'")))
  )

(define (test-parser-let)
  (define p (parse-programme (new-parser (new-lexer "let myVar = anotherVar;"))))
  
  (check-parse-errors p)
  (define programme (parser-stmts p))

  (if (not (= (length programme) 1))
      (error (format "Program did not have 3 statements. Had:" (length programme))))

  (define test (list "myVar"))
  (define index 0)
  (for-each (lambda (t)
              (define stmt (get-nth-element programme index))
              (test-let-stmt stmt t)
              (set! index (+ index 1))
              ) test))


(define (test-parser-return)
  (define p (parse-programme (new-parser (new-lexer "
return 5;
return 10;
return 993322;
"))))
  (check-parse-errors p)
  (define programme (parser-stmts p))

  (if (not (= (length programme) 3))
      (error (format "Program did not have 3 statements. Had:" (length programme))))

  (for-each (lambda (stmt) (if (not (return-node? stmt)) (error (format stmt " was not a return stmt!")))) programme)) 

(define (test-identifier-expression)
  (define p (parse-programme (new-parser (new-lexer "foobar;"))))
  (check-parse-errors p)
  
  (if (not (= (length (parser-stmts p)) 1))
      (error (format "Program did not have 1 statements. Had:" (length (parser-stmts p)))))

  (define exp (car (parser-stmts p)))
  (if (not (exp-node? exp))
      (error (format "Statement is not an  expression statement Is:" exp)))

  (if (not (string=? "foobar" (identifier-value (exp-value exp))))
      (error (format "Value not " "foobar" ". got=" (exp-value exp))))

  (if (not (string=? "foobar" (token-literal (node-token exp))))
      (error (format "Value not " "foobar" ". got=" (token-literal (node-token exp))))))

(define (test-integer-literal-expression)
  (define p (parse-programme (new-parser (new-lexer "5;"))))
  (check-parse-errors p)

  (if (not (= (length (parser-stmts p)) 1))
      (error (format "Program did not have 1 statements. Had: " (length (parser-stmts p)))))

  (define exp (car (parser-stmts p)))
  (if (not (exp-node? exp))
      (error (format "Statement is not an  expression statement Is:" exp)))

  (define literal (exp-value exp))
  (if (not (int-literal? literal))
      (error (format "exp is not an int leteral Is:" literal)))

  (if (not (= (int-value literal) 5))
      (error (format "Value is not 5, was:" (int-value literal))))

  (if (not (string=? (token-literal-from-state literal) "5"))
      (error (format "Token Literal is not 5, was:" (token-literal-from-state literal)))))

(define (test-interger-literal il value)
  (if (not (int-literal? il))
      (error (format "exp is not an int leteral. But is:" il)))

  (if (not (= value (int-value il)))
      (error (format "Integer value not: " value " got:" (int-value il))))

  (if (not (string=? (token-literal-from-state il) (number->string value)))
      (error (format "Token literal not: " value " got:" (token-literal-from-state il)))))

(define (test-identifier id value)
   (if (not (identifier-node? id))
      (error (format "exp is not an identifier. But is:" id)))

  (if (not (string=? value (identifier-value id)))
      (error (format "Identifier value not: " value " got:" (int-valueidl))))

  (if (not (string=? (token-literal-from-state id) value))
      (error (format "Token literal not: " value " got:" (token-literal-from-state il)))))

(define (test-bool b value)
  (if (not (bool-literal? b))
      (error (format "exp is not an bool. But is:" b)))

  (if (not (eq? value (bool-value b)))
      (error (format "Bool value not: " value " got:" (bool-value b)))))

(define (test-literal-expression exp expected)
  (cond ((number? expected) (test-interger-literal exp expected))
        ((string? expected) (test-identifier exp expected))
        ((bool? expected) (test-bool exp expected))))

(define (test-prefix-expressions)

  (define tests (list (list "!5" "!" 5) (list "-15" "-" 15) (list "!true" "!" #t)))

  (for-each (lambda (t)
              (define p (parse-programme (new-parser (new-lexer (car t)))))
              (check-parse-errors p)

              (if (not (= (length (parser-stmts p)) 1))
                   (error (format "Program did not have 1 statements. Had:" (length (parser-stmts p)))))

              (define stmt (car (parser-stmts p)))
              (if (not (exp-node? stmt))
                  (error (format "Statement is not an  expression statement Is:" stmt)))

              (define exp (exp-value stmt))
              (if (not (prefix-node? exp))
                  (error (format "exp is not an prefix exp. Is:" exp)))

              (if (not (char-eq? (prefix-operator exp) (cadr t)))
                  (error (format "Operator is not " (cadr t) "is not. But was" (prefix-operator exp))))

              (test-literal-expression (prefix-right exp) (caddr t))) tests))
  
(define (test-infix-expression exp left operator right)
  (if (not (infix-node? exp))
      (error (format "exp is not an infix exp. Is: " exp)))

  (test-literal-expression (infix-left exp) left)

  (if (not (or (char-eq? (infix-operator exp) operator) (string=? (infix-operator exp) operator)))
                  (error (format "Operator is not '" operator "' . But was:'" (infix-operator exp) "'")))

  (test-literal-expression (infix-right exp) right))

(define (test-infix-expressions)

  (define tests (list
                 (list "5 + 5;" 5 "+" 5)
                 (list "5 - 5;" 5 "-" 5)
                 (list "5 * 5;" 5 "*" 5)
                 (list "5 / 5;" 5 "/" 5)
                 (list "5 > 5;" 5 ">" 5)
                 (list "5 < 5;" 5 "<" 5)
                 (list "5 == 5;" 5 "==" 5)
                 (list "5 != 5;" 5 "!=" 5)
                 (list "alice * bob" "alice" "*" "bob")
                 (list "5 + 10" 5 "+" 10)
                 (list "true == true" #t "==" #t)
                 (list "false != true" #f "!=" #t)))

  (for-each (lambda (t)
              (define p (parse-programme (new-parser (new-lexer (car t)))))
              (check-parse-errors p)

              (if (not (= (length (parser-stmts p)) 1))
                   (error (format "Program did not have 1 statements. Had:" (length (parser-stmts p)))))

              (define stmt (car (parser-stmts p)))
              (if (not (exp-node? stmt))
                  (error (format "Statement is not an  expression statement Is:" stmt)))

              (define exp (exp-value stmt))
              (if (not (infix-node? exp))
                  (error (format "exp is not an infix exp. Is:" exp)))

              (test-literal-expression (infix-left exp) (cadr t))

              (if (not (or (char-eq? (infix-operator exp) (caddr t)) (string=? (infix-operator exp) (caddr t))))
                  (error (format "Operator is not '" (caddr t) "' . But was'" (infix-operator exp) "'")))

              (test-literal-expression (infix-right exp) (cadddr t))) tests) )

(define (test-bool-expressions)

  (define test (list (cons "true;" #t) (cons "false;" #f)))

  (for-each (lambda (t)
              (define p (parse-programme (new-parser (new-lexer (car t)))))
              (check-parse-errors p)

              (if (not (= (length (parser-stmts p)) 1))
                   (error (format "Program did not have 1 statements. Had:" (length (parser-stmts p)))))

              (define stmt (car (parser-stmts p)))
              (if (not (exp-node? stmt))
                  (error (format "Statement is not an  expression statement Is:" stmt)))

              (define exp (exp-value stmt))
              (if (not (bool-literal? exp))
                  (error (format "exp is not an bool-literal. Is:" exp)))

              (if (not (eq? (cdr t) (bool-value exp)))
                  (error (format "Operator is not '" (cadr t) "' . But was'" (bool-value exp) "'")))) test))

(define (test-if-expression)
  (define p (parse-programme (new-parser (new-lexer "if (x < y) { x } else { y }"))))
  (check-parse-errors p)

  (if (not (= (length (parser-stmts p)) 1))
      (error (format "Program did not have 1 statements. Had:" (length (parser-stmts p)))))

  (define stmt (car (parser-stmts p)))
  (if (not (exp-node? stmt))
      (error (format "Statement is not an expression statement Is:" stmt)))

  (define exp (exp-value stmt))
  (if (not (if-node? exp))
      (error (format "Statement is not an if expression Is:" exp)))

  (test-infix-expression (if-cond exp) "x" "<" "y")

  (define cons-stmts (block-stmts (if-cons exp)))
  (if (not (= (length cons-stmts) 1))
      (error (format "Cons did not have 1 statement. Had:" (length cons-stmts))))

  (define cons (car cons-stmts))
  (if (not (exp-node? cons))
      (error (format "Cons statement is not an expression statement Is:" cons)))

  (test-identifier (exp-value cons) "x")

  (define alt-stmt (block-stmts (if-alt exp)))
  (if (not (= (length alt-stmt) 1))
      (error (format "Alt did not have 1 statement. Had:" (length alt-stmt))))

  (define alt (car alt-stmt))
  (if (not (exp-node? alt))
      (error (format "Alt statement is not an expression statement Is:" alt)))

  (test-identifier (exp-value alt) "y"))

(define (test-function-literal)
  (define p (parse-programme (new-parser (new-lexer "fn(x, y) { x + y; }"))))
  (check-parse-errors p)

  (if (not (= (length (parser-stmts p)) 1))
      (error (format "Program did not have 1 statements. Had:" (length (parser-stmts p)))))

  (define stmt (car (parser-stmts p)))
  (if (not (exp-node? stmt))
      (error (format "Statement is not an expression statement Is:" stmt)))

  (define fn (exp-value stmt))
  (if (not (fn? fn))
      (error (format "Statement is not an fn literal. Is:" fn)))

  (define params (fn-params fn))
  (if (not (= (length params) 2))
      (error (format "Fn has wrong params. Want 2, got:" fn)))

  (test-literal-expression (car params) "x")
  (test-literal-expression (cadr params) "y") 

  (define body (block-stmts (fn-body fn)))
  (if (not (= (length body) 1))
      (error (format "Fn has wrong body. Want 1 stmt, got:" (length body))))

  (define body-stmt (car body))
  (if (not (exp-node? body-stmt))
      (error (format "Body is not an expression statement Is:" body-stmt)))

  (test-infix-expression (exp-value body-stmt) "x" "+" "y"))

(define (test-functions-parameters)
  (define tests (list
                 (list "fn() {}" '())
                 (list "fn(x) {}" (list "x"))
                 (list "fn(x, y, z) {}" (list "x" "y" "z"))
                 ))

  (for-each (lambda (t)
              (define p (parse-programme (new-parser (new-lexer (car t)))))
              (check-parse-errors p)

              (define stmt (car (parser-stmts p)))
              (define fn (exp-value stmt))
              (if (not (fn? fn))
                  (error (format "Statement is not an fn literal. Is:" fn)))
              
              (define params (fn-params fn))
              (if (not (= (length params) (length (cadr t))))
                  (error (format "Length of paramers wrong want" (length (cadr t) "but got" (length params)))))
              
              (define index 0)
              (for-each (lambda (param) (test-literal-expression (get-nth-element params index) param) (set! index (+ index 1))) (cadr t))) tests))

(define (test-call-expressions)
  (define p (parse-programme (new-parser (new-lexer "add(1, 2 * 3, 4 + 5);"))))
  (check-parse-errors p)

  (if (not (= (length (parser-stmts p)) 1))
      (error (format "Program did not have 1 statements. Had:" (length (parser-stmts p)))))

  (define stmt (car (parser-stmts p)))
  (if (not (exp-node? stmt))
      (error (format "Statement is not an expression statement Is:" stmt)))

  (define c (exp-value stmt))
  (if (not (call-exp? c))
      (error (format "Statement is not a call exp. Is:" c)))

  (test-identifier (call-fn c) "add")

  (define params (call-args c))
  (if (not (= (length params) 3))
      (error (format "Length of paramers wrong want 3 but got" (length params))))

  (test-literal-expression (get-nth-element params 0) 1)
  (test-infix-expression (get-nth-element params 1) 2 "*" 3)
  (test-infix-expression (get-nth-element params 2) 4 "+" 5))

(define (test-let-statements)
  (define tests (list
                 (list "let x = 5;" "x" 5)
                 (list "let y = true;" "y" #t)
                 (list "let foobar = y;" "foobar" "y")
                 ))

  (for-each (lambda (t)
              (define p (parse-programme (new-parser (new-lexer (car t)))))
              (check-parse-errors p)

              (if (not (= (length (parser-stmts p)) 1))
                  (error (format "Program did not have 1 statements. Had:" (length (parser-stmts p)))))

              (define let-stmt (car (parser-stmts p)))
              (test-let-stmt let-stmt (cadr t))
              (test-literal-expression (let-value let-stmt) (caddr t))) tests))

(display-nl "Starting parser tests...")
(test-parser-let)
(test-parser-return)
(test-identifier-expression)
(test-integer-literal-expression)
(test-prefix-expressions)
(test-infix-expressions)
(test-bool-expressions)
(test-if-expression)
(test-function-literal)
(test-functions-parameters)
(test-call-expressions)
(test-let-statements)
(display-nl "\tParser tests have passed without errros")