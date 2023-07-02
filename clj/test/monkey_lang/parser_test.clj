(ns monkey-lang.parser-test
  (:require [clojure.test :refer [deftest is testing]]
            [monkey-lang.parser :as parser]
            [monkey-lang.ast :as ast]))

(deftest statement-test
  (testing "let"
    (is (= "let x = 5;\n" (ast/to-str (parser/run "let x = 5;")))))
  (testing "return"
    (is (= "return 10;\n" (ast/to-str (parser/run "return 10;")))))
  (testing "expression"
    (is (= "((5 * 5) + 10);\n" (ast/to-str (parser/run "5 * 5 + 10;"))))))

(deftest literal-test
  (testing "identifier"
    (is (= "foobar;\n" (ast/to-str (parser/run "foobar;")))))
  (testing "integer"
    (is (= "100;\n" (ast/to-str (parser/run "100;")))))
  (testing "boolean"
    (is (= "true;\n" (ast/to-str (parser/run "true;")))))
  (testing "string"
    (is (= "\"Thorsten\";\n" (ast/to-str (parser/run "\"Thorsten\";"))))))

(deftest expresion-test
  (testing "prefix"
    (is (= "(!true);\n" (ast/to-str (parser/run "!true;")))))
  (testing "infix"
    (is (= "((5 * 3) + 3);\n" (ast/to-str (parser/run "5 * 3 + 3;")))))
  (testing "if"
    (is (= "if ((x < y)) {\n  x;\n} else {\n  y;\n};\n" (ast/to-str (parser/run "if (x < y) { x } else { y };")))))
  (testing "fn"
    (is (= "fn (x, y) {\n  (x + y);\n};\n" (ast/to-str (parser/run "fn (x, y) { x + y };")))))
  (testing "call"
    (is (= "add(1, (2 * 3), (4 + 5));\n" (ast/to-str (parser/run "add(1, 2 * 3, 4 + 5);"))))))

(deftest parse-error-test
  (testing "let error"
    (is (= (with-out-str (println "ParseError \n Expected: :assign \n    Found: [:illegal |] \n Location: 1 : 10 (line : col) \n \n let hello | 5; \n           ^ \n")  )
           (with-out-str (parser/print-error (try (parser/run "let hello | 5;") (catch clojure.lang.ExceptionInfo e e))))))
    (is (= (with-out-str (println "ParseError \n Expected: :l_squirly \n    Found: [:ident x] \n Location: 1 : 29 (line : col) \n \n callsFunction(2, 3, fn(x, y) x + y; }); \n                              ^ \n")  )
           (with-out-str (parser/print-error (try (parser/run "callsFunction(2, 3, fn(x, y) x + y; });") (catch clojure.lang.ExceptionInfo e e))))))))
