(ns monkey.lexer-test
  (:require [clojure.test :refer [deftest testing is]]
            [monkey.lexer :refer [lexer next-token]]
            [monkey.token :refer [new-token token]]))

(def input "let five = 5;
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
           ")

(deftest tokeniser-test
  
  (testing "with test tokens" 
    (let [tests (map (partial apply new-token)
                     [[(:let token) "let"]
                      [(:ident token) "five"]
                      [(:assign token) "="]
                      [(:int token) "5"]
                      [(:semicolon token) ";"]
                      [(:let token) "let"]
                      [(:ident token) "ten"]
                      [(:assign token) "="]
                      [(:int token) "10"]
                      [(:semicolon token) ";"]
                      [(:let token) "let"]
                      [(:ident token) "add"]
                      [(:assign token) "="]
                      [(:fn token) "fn"]
                      [(:lparen token) "("]
                      [(:ident token) "x"]
                      [(:comma token) ","]
                      [(:ident token) "y"]
                      [(:rparen token) ")"]
                      [(:lbrace token) "{"]
                      [(:ident token) "x"]
                      [(:plus token) "+"]
                      [(:ident token) "y"]
                      [(:semicolon token) ";"]
                      [(:rbrace token) "}"]
                      [(:semicolon token) ";"]
                      [(:let token) "let"]
                      [(:ident token) "result"]
                      [(:assign token) "="]
                      [(:ident token) "add"]
                      [(:lparen token) "("]
                      [(:ident token) "five"]
                      [(:comma token) ","]
                      [(:ident token) "ten"]
                      [(:rparen token) ")"]
                      [(:semicolon token) ";"]
                      [(:bang token) "!"]
                      [(:minus token) "-"]
                      [(:slash token) "/"]
                      [(:asterisk token) "*"]
                      [(:int token) "5"]
                      [(:semicolon token) ";"]
                      [(:int token) "5"]
                      [(:lt token) "<"]
                      [(:int token) "10"]
                      [(:gt token) ">"]
                      [(:int token) "5"]
                      [(:semicolon token) ";"]
                      [(:if token) "if"]
                      [(:lparen token) "("]
                      [(:int token) "5"]
                      [(:lt token) "<"]
                      [(:int token) "10"]
                      [(:rparen token) ")"]
                      [(:lbrace token) "{"]
                      [(:return token) "return"]
                      [(:true token) "true"]
                      [(:semicolon token) ";"]
                      [(:rbrace token) "}"]
                      [(:else token) "else"]
                      [(:lbrace token) "{"]
                      [(:return token) "return"]
                      [(:false token) "false"]
                      [(:semicolon token) ";"]
                      [(:rbrace token) "}"]
                      [(:int token) "10"]
                      [(:eq token) "=="]
                      [(:int token) "10"]
                      [(:semicolon token) ";"]
                      [(:int token) "10"]
                      [(:not-eq token) "!="]
                      [(:int token) "9"]
                      [(:semicolon token) ";"]
                      [(:eof token) ""]])
          tokens (loop [tokens (rest (iterate next-token (lexer input)))
                        result []]
                   (if (= (get-in (first tokens) [:token :type]) (:eof token))
                     (conj result (:token (first tokens)))
                     (recur (rest tokens)
                            (conj result (:token (first tokens))))))]
      (doseq [n (range (count tests))]
        (is (= (nth tests n) (nth tokens n)))))))
