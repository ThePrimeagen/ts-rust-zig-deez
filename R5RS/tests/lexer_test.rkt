(#%require (only racket/base time error))
(load "../src/utils.rkt")
(loader "token")
(loader "lexer")



(define (test-new-token)
  (define tests 
            (list 
                (cons LET "let")
                (cons IDENT "five")
                (cons ASSIGN "=")
                (cons INT "5")
                (cons SEMICOLON ";")
                (cons LET "let")
                (cons IDENT "ten")
                (cons ASSIGN "=")
                (cons INT "10")
                (cons SEMICOLON ";")
                (cons LET "let")
                (cons IDENT "add")
                (cons ASSIGN "=")
                (cons FUNCTION "fn")
                (cons LPAREN "(")
                (cons IDENT "x")
                (cons COMMA ",")
                (cons IDENT "y")
                (cons RPAREN ")")
                (cons LBRACE "{")
                (cons IDENT "x")
                (cons PLUS "+")
                (cons IDENT "y")
                (cons SEMICOLON ";")
                (cons RBRACE "}")
                (cons SEMICOLON ";")
                (cons LET "let")
                (cons IDENT "result")
                (cons ASSIGN "=")
                (cons IDENT "add")
                (cons LPAREN "(")
                (cons IDENT "five")
                (cons COMMA ",")
                (cons IDENT "ten")
                (cons RPAREN ")")
                (cons SEMICOLON ";")
                (cons BANG "!")
                (cons MINUS "-")
                (cons SLASH "/")
                (cons ASTERISK "*")
                (cons INT "5")
                (cons SEMICOLON ";")
                (cons INT "5")
                (cons LT "<")
                (cons INT "10")
                (cons GT ">")
                (cons INT "5")
                (cons SEMICOLON ";")
                (cons IF_ "if")
                (cons LPAREN "(")
                (cons INT "5")
                (cons LT "<")
                (cons INT "10")
                (cons RPAREN ")")
                (cons LBRACE "{")
                (cons RETURN "return")
                (cons TRUE "true")
                (cons SEMICOLON ";")
                (cons RBRACE "}")
                (cons ELSE_ "else")
                (cons LBRACE "{")
                (cons RETURN "return")
                (cons FALSE "false")
                (cons SEMICOLON ";")
                (cons RBRACE "}")
                (cons INT "10")
                (cons EQ "==")
                (cons INT "10")
                (cons SEMICOLON ";")
                (cons MONKEY_STRING "foobar ")
                (cons INT "10")
                (cons NOT_EQ "!=")
                (cons INT "9")
                (cons SEMICOLON ";")
                (cons MONKEY_STRING "foo \" bar")
                (cons SEMICOLON ";")
                (cons LBRACKET "[")
                (cons INT "1")
                (cons COMMA ",")
                (cons INT "2")
                (cons RBRACKET "]")
                (cons SEMICOLON ";")
                (cons LBRACE "{")
                (cons MONKEY_STRING "foo")
                (cons COLON ":")
                (cons MONKEY_STRING "bar")
                (cons RBRACE "}")
                (cons EOF "")
            ))
  (define l (new-lexer "let five = 5;
let ten = 10;
   let add = fn(x, y) {
     x + y;
};
   let result = add(five, ten);
!-/*5;
   5 < 10 > 5;

   if (5 < 10) {
       return true;
   } else {
       return false;}
10 == 10;
\"foobar \"
10 != 9;
\"foo \\\" bar\";
[1, 2];
{\"foo\": \"bar\"}
"))

  
  (define index 0)

  (for-each (lambda (token)
              (define t (next-token l))
              (cond (
                     (not (eq? (token-type t) (car token)))
                     (error (format "tests[" index "] - tokentype wrong. expected:\"" (car token) "\" got:\"" (token-type t) "\""))
                     )
                    (
                     (not (or (char-eq? (token-literal t) (cdr token)) (string=? (token-literal t) (cdr token))))
                     (error (format "tests[" index "] - literal wrong. expected '" (cdr token) "' got '" (token-literal t) "'"))
                     ))
              (set! index (+ index 1))) tests))

(display-nl "Starting lexer tests...")
(test-new-token)
(display-nl "\tLexer tests have passed without errros")