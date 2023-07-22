(ns monkey-lang.lexer-test
  (:require [clojure.test :refer [deftest is testing]]
            [monkey-lang.token :as token]
            [monkey-lang.lexer :as lexer]))

(def program 
  "
  let five = 5;
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
    return false;
  }

  10 == 10;
  10 != 9;
  \"foobar\"
  \"foo bar\"
  [1, 2];
  {\"foo\": \"bar\"}
  null
  let x = null
  ")

(deftest lexer-test
  (testing "=+(){},;"
    (let [result [[token/LET "let"]
                  [token/IDENT "five"]
                  [token/ASSIGN "="]
                  [token/INT "5"]
                  [token/SEMICOLON ";"]
                  [token/LET "let"]
                  [token/IDENT "ten"]
                  [token/ASSIGN "="]
                  [token/INT "10"]
                  [token/SEMICOLON ";"]
                  [token/LET "let"]
                  [token/IDENT "add"]
                  [token/ASSIGN "="]
                  [token/FUNCTION "fn"]
                  [token/LPAREN "("]
                  [token/IDENT "x"]
                  [token/COMMA ","]
                  [token/IDENT "y"]
                  [token/RPAREN ")"]
                  [token/LBRACE "{"]
                  [token/IDENT "x"]
                  [token/PLUS "+"]
                  [token/IDENT "y"]
                  [token/SEMICOLON ";"]
                  [token/RBRACE "}"]
                  [token/SEMICOLON ";"]
                  [token/LET "let"]
                  [token/IDENT "result"]
                  [token/ASSIGN "="]
                  [token/IDENT "add"]
                  [token/LPAREN "("]
                  [token/IDENT "five"]
                  [token/COMMA ","]
                  [token/IDENT "ten"]
                  [token/RPAREN ")"]
                  [token/SEMICOLON ";"]
                  [token/BANG "!"]
                  [token/MINUS "-"]
                  [token/SLASH "/"]
                  [token/ASTERISK "*"]
                  [token/INT "5"]
                  [token/SEMICOLON ";"]
                  [token/INT "5"]
                  [token/LT "<"]
                  [token/INT "10"]
                  [token/GT ">"]
                  [token/INT "5"]
                  [token/SEMICOLON ";"]
                  [token/IF "if"]
                  [token/LPAREN "("]
                  [token/INT "5"]
                  [token/LT "<"]
                  [token/INT "10"]
                  [token/RPAREN ")"]
                  [token/LBRACE "{"]
                  [token/RETURN "return"]
                  [token/TRUE "true"]
                  [token/SEMICOLON ";"]
                  [token/RBRACE "}"]
                  [token/ELSE "else"]
                  [token/LBRACE "{"]
                  [token/RETURN "return"]
                  [token/FALSE "false"]
                  [token/SEMICOLON ";"]
                  [token/RBRACE "}"]
                  [token/INT "10"]
                  [token/EQ "=="]
                  [token/INT "10"]
                  [token/SEMICOLON ";"]
                  [token/INT "10"]
                  [token/NOT_EQ "!="]
                  [token/INT "9"]
                  [token/SEMICOLON ";"]
                  [token/STRING "foobar"]
                  [token/STRING "foo bar"]
                  [token/LBRACKET "["]
                  [token/INT "1"]
                  [token/COMMA ","]
                  [token/INT "2"]
                  [token/RBRACKET "]"]
                  [token/SEMICOLON ";"]
                  [token/LBRACE "{"]
                  [token/STRING "foo"]
                  [token/COLON ":"]
                  [token/STRING "bar"]
                  [token/RBRACE "}"]
                  [token/NULL "null"]
                  [token/LET "let"]
                  [token/IDENT "x"]
                  [token/ASSIGN "="]
                  [token/NULL "null"]
                  [token/EOF ""]]]
    (is (= result (lexer/run program))))))
