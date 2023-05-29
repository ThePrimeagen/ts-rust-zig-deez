(ns clj.lexer-test
  (:require [clojure.test :refer [deftest is testing]]
            [clj.util :refer [to-str]]
            [clj.token :as token]
            [clj.lexer :refer [lex]]))

(deftest lexer-test
  (testing "=+(){},;"
    (let [result [[:assign    \=  0]
                  [:plus      \+  1]
                  [:l_paren   \(  2]
                  [:r_paren   \)  3]
                  [:l_squirly \{  4]
                  [:r_squirly \}  5]
                  [:comma     \,  6]
                  [:semicolon \;  7]
                  [:eof       nil 8]]]
    (is (= (lex "=+(){},;") result))))

  (testing "input.monkey with literals only"
    (let [input (slurp "./test/clj/input.monkey")
          deez  (->> (lex input)
                     (mapv token/literal)
                     (mapv to-str)
                     (butlast)) ;; skipping eof chr
          nuts  ["let" "five" "=" "5" ";" "let" "ten" "=" "10" ";" "let" "add" "=" "fn" "(" "x" "," "y" ")" "{" "x" "+" "y" ";" "}" ";" "let" "result" "=" "add" "(" "five" "," "ten" ")" ";"]]
    (is (= deez nuts))))

  (testing "input.monkey with position"
    (let [result [[:let "let" 0] 
                  [:ident "five" 4] 
                  [:assign \= 9] 
                  [:int "5" 11] 
                  [:semicolon \; 12] 
                  [:let "let" 15] 
                  [:ident "ten" 19] 
                  [:assign \= 23] 
                  [:int "10" 25] 
                  [:semicolon \; 27] 
                  [:let "let" 30] 
                  [:ident "add" 34] 
                  [:assign \= 38] 
                  [:fn "fn" 40] 
                  [:l_paren \( 42] 
                  [:ident "x" 43] 
                  [:comma \, 44] 
                  [:ident "y" 46] 
                  [:r_paren \) 47] 
                  [:l_squirly \{ 49] 
                  [:ident "x" 54] 
                  [:plus \+ 56] 
                  [:ident "y" 58] 
                  [:semicolon \; 59] 
                  [:r_squirly \} 62] 
                  [:semicolon \; 63] 
                  [:let "let" 66] 
                  [:ident "result" 70] 
                  [:assign \= 77] 
                  [:ident "add" 79] 
                  [:l_paren \( 82] 
                  [:ident "five" 83] 
                  [:comma \, 87] 
                  [:ident "ten" 89] 
                  [:r_paren \) 92] 
                  [:semicolon \; 93] 
                  [:eof nil 94]]]
    (is (= (lex (slurp "./test/clj/input.monkey")) result)))))
