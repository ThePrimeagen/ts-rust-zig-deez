(ns monkey-lang.lexer-test
  (:require [clojure.test :refer [deftest is testing]]
            [monkey-lang.util :refer [to-str]]
            [monkey-lang.token :as token]
            [monkey-lang.lexer :as lexer]))

(def program 
  "let five = 5;
   let ten = 10;
   let add = fn(x, y) {
     x + y;
   };
   let result = add(five, ten);")

(deftest lexer-test
  (testing "=+(){},;"
    (let [result [[:assign    "=" ]
                  [:plus      "+" ]
                  [:l_paren   "(" ]
                  [:r_paren   ")" ]
                  [:l_squirly "{" ]
                  [:r_squirly "}" ]
                  [:comma     "," ]
                  [:semicolon ";" ]
                  [:eof       ""]]]
    (is (= (lexer/run "=+(){},;") result))))

  (testing "program with literals only"
    (let [deez  (->> (lexer/run program)
                     (mapv token/literal)
                     (mapv to-str)) ;; skipping eof chr
          nuts  ["let" "five" "=" "5" ";" "let" "ten" "=" "10" ";" "let" "add" "=" "fn" "(" "x" "," "y" ")" "{" "x" "+" "y" ";" "}" ";" "let" "result" "=" "add" "(" "five" "," "ten" ")" ";" ""]]
    (is (= deez nuts))))

  (testing "program with position"
    (let [result [[:let "let"] 
                  [:ident "five"] 
                  [:assign "="] 
                  [:int "5"] 
                  [:semicolon ";"] 
                  [:let "let"] 
                  [:ident "ten"] 
                  [:assign "="] 
                  [:int "10"] 
                  [:semicolon ";"] 
                  [:let "let"] 
                  [:ident "add"] 
                  [:assign "="] 
                  [:fn "fn"] 
                  [:l_paren "("] 
                  [:ident "x"] 
                  [:comma ","] 
                  [:ident "y"] 
                  [:r_paren ")"] 
                  [:l_squirly "{"] 
                  [:ident "x"] 
                  [:plus "+"] 
                  [:ident "y"] 
                  [:semicolon ";"] 
                  [:r_squirly "}"] 
                  [:semicolon ";"] 
                  [:let "let"] 
                  [:ident "result"] 
                  [:assign "="] 
                  [:ident "add"] 
                  [:l_paren "("] 
                  [:ident "five"] 
                  [:comma ","] 
                  [:ident "ten"] 
                  [:r_paren ")"] 
                  [:semicolon ";"] 
                  [:eof ""]]]
    (is (= (lexer/run program) result)))))
