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
  (if (not (let-stmt? stmt))
      (error (format stmt " was not a let stmt!")))

  (if (not (string=? (id-value (let-id stmt)) name))
      (error (format "Wrong name.value, not :'" name "' got: '"  (id-value (let-id stmt)) "'")))
  )

(define (test-parser-let)
  (define p (parse-programme (new-parser (new-lexer "let myVar = anotherVar;"))))
  
  (check-parse-errors p)
  (define programme (parser-stmts p))

  (if (not (= (length programme) 1))
      (error (format "Program did not have 3 statements. Had: " (length programme))))

  (define test (list "myVar"))
  (define index 0)
  (for-each (lambda (t)
              (define stmt (get-nth-element programme index))
              (test-let-stmt stmt t)
              (set! index (+ index 1))
              ) test)
  ;(display-l p)
  )


(define (test-parser-return)
  (define p (parse-programme (new-parser (new-lexer "
return 5;
return 10;
return 993322;
"))))

  (check-parse-errors p)
  (define programme (parser-stmts p))

  (if (not (= (length programme) 3))
      (error (format "Program did not have 3 statements. Had: " (length programme))))

  (for-each (lambda (stmt)
              (if (not (return-stmt? stmt))
                  (error (format stmt " was not a return stmt!")))

              
              ) programme)
  ;(display-l programme)
  ) 

(define (test-identifier-expression)
  (define p (parse-programme (new-parser (new-lexer "foobar;"))))
  (check-parse-errors p)
  
  (if (not (= (length (parser-stmts p)) 1))
      (error (format "Program did not have 1 statements. Had: " (length (parser-stmts p)))))

  (define exp (car (parser-stmts p)))
  (if (not (expression-stmt? exp))
      (error (format "Statement is not an  expression statement Is: " exp)))

  (if (not (string=? "foobar" (id-value (expression-value exp))))
      (error (format "Value not " "foobar" ". got=" (expression-value exp))))

  (if (not (string=? "foobar" (token-literal (expression-token exp))))
      (error (format "Value not " "foobar" ". got=" (token-literal (expression-token exp)))))
  )

(define (test-integer-literal-expression)
  (define p (parse-programme (new-parser (new-lexer "5;"))))
  (check-parse-errors p)

  (if (not (= (length (parser-stmts p)) 1))
      (error (format "Program did not have 1 statements. Had: " (length (parser-stmts p)))))

  (define exp (car (parser-stmts p)))
  (if (not (expression-stmt? exp))
      (error (format "Statement is not an  expression statement Is: " exp)))

  (define literal (expression-value exp))
  (if (not (int-literal? literal))
      (error (format "exp is not an int leteral Is: " literal)))

  (if (not (= (int-value literal) 5))
      (error (format "Value is not 5, was: " (int-value literal))))

  (if (not (string=? (token-literal-from-state literal) "5"))
      (error (format "Token Literal is not 5, was: " (token-literal-from-state literal))))
  ;(display-l (parser-stmts p))
  )

(define (test-interger-literal il value)
  (if (not (int-literal? il))
      (error (format "exp is not an int leteral. But is: " il)))

  (if (not (= value (int-value il)))
      (error (format "Integer value not: " value " got: " (int-value il))))

  (if (not (string=? (token-literal-from-state il) (number->string value)))
      (error (format "Token literal not: " value " got: " (token-literal-from-state il))))
  )

(define (test-identifier id value)
   (if (not (id-stmt? id))
      (error (format "exp is not an identifier. But is: " id)))

  (if (not (= value (id-value id)))
      (error (format "Identifier value not: " value " got: " (int-valueidl))))

  (if (not (string=? (token-literal-from-state id) (number->string value)))
      (error (format "Token literal not: " value " got: " (token-literal-from-state il))))
  )

(define (test-bool b value)
  (if (not (bool-literal? id))
      (error (format "exp is not an bool. But is: " id)))

  (if (not (eq? value (int-value il)))
      (error (format "Integer value not: " value " got: " (int-value il))))

  (if (not (string=? (token-literal-from-state il) (number->string value)))
      (error (format "Token literal not: " value " got: " (token-literal-from-state il))))

  )

(define (text-literal-expression exp expected)
  (cond (number? expected) (test-interger-literal exp expected)
        (string? expected) (test-identifier exp expected)
        (bool? expected) (test-bool exp expected)))

(define (test-prefix-expressions)

  (define tests (list (list "!5" "!" 5) (list "-15" "-" 15) (list "!true" "!" #t)))

  (for-each (lambda (t)
              (define p (parse-programme (new-parser (new-lexer (car t)))))
              (check-parse-errors p)

              (if (not (= (length (parser-stmts p)) 1))
                   (error (format "Program did not have 1 statements. Had: " (length (parser-stmts p)))))

              (define stmt (car (parser-stmts p)))
              (if (not (expression-stmt? stmt))
                  (error (format "Statement is not an  expression statement Is: " stmt)))

              (define exp (expression-value stmt))
              (if (not (prefix-exp? exp))
                  (error (format "exp is not an prefix exp. Is: " exp)))

              (if (not (char-eq? (prefix-exp-operator exp) (cadr t)))
                  (error (format "Operator is not " (cadr t) "is not. But was " (prefix-exp-operator exp))))

              (text-literal-expression (prefix-exp-right exp) (caddr t))
              
              ) tests)
  )

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
                   (error (format "Program did not have 1 statements. Had: " (length (parser-stmts p)))))

              (define stmt (car (parser-stmts p)))
              (if (not (expression-stmt? stmt))
                  (error (format "Statement is not an  expression statement Is: " stmt)))

              (define exp (expression-value stmt))
              (if (not (infix-exp? exp))
                  (error (format "exp is not an infix exp. Is: " exp)))

              (text-literal-expression (infix-exp-left exp) (cadr t))

              (if (not (or (char-eq? (infix-exp-operator exp) (caddr t)) (string=? (infix-exp-operator exp) (caddr t))))
                  (error (format "Operator is not '" (caddr t) "' . But was '" (infix-exp-operator exp) "'")))

              (text-literal-expression (infix-exp-right exp) (cadddr t))
              
              ) tests)
  )

(define (test-bool-expressions)

  (define test (list (cons "true;" #t) (cons "false;" #f)))

  (for-each (lambda (t)
              (define p (parse-programme (new-parser (new-lexer (car t)))))
              (check-parse-errors p)

              (if (not (= (length (parser-stmts p)) 1))
                   (error (format "Program did not have 1 statements. Had: " (length (parser-stmts p)))))

              (define stmt (car (parser-stmts p)))
              (if (not (expression-stmt? stmt))
                  (error (format "Statement is not an  expression statement Is: " stmt)))

              (define exp (expression-value stmt))
              (if (not (bool-literal? exp))
                  (error (format "exp is not an bool-literal. Is: " exp)))

              (if (not (eq? (cdr t) (bool-value exp)))
                  (error (format "Operator is not '" (cadr t) "' . But was '" (bool-value exp) "'")))
              
              ) test)
  )

(test-parser-let)
(test-parser-return)
(test-identifier-expression)
(test-integer-literal-expression)
(test-prefix-expressions)
(test-infix-expressions)
(test-bool-expressions)