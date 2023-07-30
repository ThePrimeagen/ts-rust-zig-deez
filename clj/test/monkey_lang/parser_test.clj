(ns monkey-lang.parser-test
  (:require [clojure.test :refer [deftest is testing]]
            [monkey-lang.parser :as parser]
            [monkey-lang.ast :as ast]
            [monkey-lang.util :as util]))

(def parse (comp ast/to-str parser/run))

(deftest parser-test
  (testing "let statements"
    (is (= "let x = 5;\n" (parse "let x = 5;")))
    (is (= "let y = true;\n" (parse "let y = true;")))
    (is (= "let foobar = y;\n" (parse "let foobar = y;"))))
  (testing "return statements"
    (is (= "return 5;\n" (parse "return 5;")))
    (is (= "return true;\n" (parse "return true;")))
    (is (= "return foobar;\n" (parse "return foobar;"))))
  (testing "identifier statements"
    (is (= "foobar;\n" (parse "foobar;"))))
  (testing "integer literal expression"
    (is (= "5;\n" (parse "5;"))))
  (testing "prefix expression"
    (is (= "(!5);\n" (parse "!5;")))
    (is (= "(-15);\n" (parse "-15;")))
    (is (= "(!foobar);\n" (parse "!foobar;")))
    (is (= "(-foobar);\n" (parse "-foobar;")))
    (is (= "(!true);\n" (parse "!true;")))
    (is (= "(!false);\n" (parse "!false;"))))
  (testing "infix expression"
    (is (= "(5 + 5);\n" (parse "5 + 5;")))
    (is (= "(5 - 5);\n" (parse "5 - 5;")))
    (is (= "(5 * 5);\n" (parse "5 * 5;")))
    (is (= "(5 / 5);\n" (parse "5 / 5;")))
    (is (= "(5 > 5);\n" (parse "5 > 5;")))
    (is (= "(5 < 5);\n" (parse "5 < 5;")))
    (is (= "(5 == 5);\n" (parse "5 == 5;")))
    (is (= "(5 != 5);\n" (parse "5 != 5;")))
    (is (= "(foobar + barfoo);\n" (parse "foobar + barfoo;")))
    (is (= "(foobar - barfoo);\n" (parse "foobar - barfoo;")))
    (is (= "(foobar * barfoo);\n" (parse "foobar * barfoo;")))
    (is (= "(foobar / barfoo);\n" (parse "foobar / barfoo;")))
    (is (= "(foobar > barfoo);\n" (parse "foobar > barfoo;")))
    (is (= "(foobar < barfoo);\n" (parse "foobar < barfoo;")))
    (is (= "(foobar == barfoo);\n" (parse "foobar == barfoo;")))
    (is (= "(foobar != barfoo);\n" (parse "foobar != barfoo;")))
    (is (= "(true == true);\n" (parse "true == true")))
    (is (= "(true != false);\n" (parse "true != false")))
    (is (= "(false == false);\n" (parse "false == false"))))
  (testing "operator precedence"
    (is (= "((-a) * b);\n" (parse "-a * b")))
    (is (= "(!(-a));\n" (parse "!-a")))
    (is (= "((a + b) + c);\n" (parse "a + b + c")))
    (is (= "((a + b) - c);\n" (parse "a + b - c")))
    (is (= "((a * b) * c);\n" (parse "a * b * c")))
    (is (= "((a * b) / c);\n" (parse "a * b / c")))
    (is (= "(a + (b / c));\n" (parse "a + b / c")))
    (is (= "(((a + (b * c)) + (d / e)) - f);\n" (parse "a + b * c + d / e - f")))
    (is (= "(3 + 4);\n((-5) * 5);\n" (parse "3 + 4; -5 * 5")))
    (is (= "((5 > 4) == (3 < 4));\n" (parse "5 > 4 == 3 < 4")))
    (is (= "((5 < 4) != (3 > 4));\n" (parse "5 < 4 != 3 > 4")))
    (is (= "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)));\n" (parse "3 + 4 * 5 == 3 * 1 + 4 * 5")))
    (is (= "true;\n" (parse "true")))
    (is (= "false;\n" (parse "false")))
    (is (= "((3 > 5) == false);\n" (parse "3 > 5 == false")))
    (is (= "((3 < 5) == true);\n" (parse "3 < 5 == true")))
    (is (= "((1 + (2 + 3)) + 4);\n" (parse "1 + (2 + 3) + 4")))
    (is (= "((5 + 5) * 2);\n" (parse "(5 + 5) * 2")))
    (is (= "(2 / (5 + 5));\n" (parse "2 / (5 + 5)")))
    (is (= "(((5 + 5) * 2) * (5 + 5));\n" (parse "(5 + 5) * 2 * (5 + 5)")))
    (is (= "(-(5 + 5));\n" (parse "-(5 + 5)")))
    (is (= "(!(true == true));\n" (parse "!(true == true)")))
    (is (= "((a + add((b * c))) + d);\n" (parse "a + add(b * c) + d")))
    (is (= "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)));\n" (parse "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))")))
    (is (= "add((((a + b) + ((c * d) / f)) + g));\n" (parse "add(a + b + c * d / f + g)")))
    (is (= "((a * ([1, 2, 3, 4][(b * c)])) * d);\n" (parse "a * [1, 2, 3, 4][b * c] * d")))
    (is (= "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])));\n" (parse "add(a * b[2], b[1], 2 * [1, 2][1])"))))
  (testing "boolean expression"
    (is (= "true;\n" (parse "true;")))
    (is (= "false;\n" (parse "false;"))))
  (testing "if statement"
    (is (= "if ((x < y)) {\n  x;\n};\n" (parse "if (x < y) { x }"))))
  (testing "if else statement"
    (is (= "if ((x < y)) {\n  x;\n} else {\n  y;\n};\n" (parse "if (x < y) { x } else { y }"))))
  (testing "function literals"
    (is (= "fn (x, y) {\n  (x + y);\n};\n" (parse "fn(x, y) { x + y; }")))
    (is (= "fn () { };\n" (parse "fn() {}")))
    (is (= "fn (x) { };\n" (parse "fn(x) { }")))
    (is (= "fn (x, y, z) { };\n" (parse "fn(x, y, z) { }")))
  (testing "call expression"
    (is (= "add(1, (2 * 3), (4 + 5));\n" (ast/to-str (parser/run "add(1, 2 * 3, 4 + 5);"))))))
  (testing "string literal"
    (is (= "\"hello world\";\n" (parse "\"hello world\";"))))
  (testing "array literals"
    (is (= "[];\n" (parse "[]")))
    (is (= "[1, (2 * 2), (3 + 3)];\n" (parse "[1, 2 * 2, 3 + 3]"))))
  (testing "hash literals"
    (is (= "{};\n" (parse "{}")))
    (is (= "{\"one\": 1, \"two\": 2, \"three\": 3};\n" (parse "{\"one\": 1, \"two\": 2, \"three\": 3}")))
    (is (= "{true: 1, false: 2};\n" (parse "{true: 1, false: 2}")))
    (is (= "{1: 1, 2: 2, 3: 3};\n" (parse "{1: 1, 2: 2, 3: 3}")))
    (is (= "{\"one\": (0 + 1), \"two\": (10 - 8), \"three\": (15 / 5)};\n" (parse "{\"one\": 0 + 1, \"two\": 10 - 8, \"three\": 15 / 5}"))))
  (testing "index operator"
    (is (= "([1, 2, 3, 4, 5][(1 * 3)]);\n" (parse "[1,2,3,4,5][1*3]")))
    (is (= "({1: 2, 2: 3, 3: 4}[(1 * 3)]);\n" (parse "{1: 2, 2: 3, 3: 4}[(1*3)]"))))
  (testing "error testing"
    (is (= (with-out-str (println (util/yellow "ParseError") "\n Expected: :token/assign \n    Found: [:token/illegal |] \n Location: 1 : 10 (line : col) \n \n let hello | 5; \n          " (util/yellow "^")))
           (with-out-str (parser/print-error (try (parser/run "let hello | 5;") (catch clojure.lang.ExceptionInfo e e))))))
    (is (= (with-out-str (println (util/yellow "ParseError") "\n Expected: :token/l-brace \n    Found: [:token/ident x] \n Location: 1 : 29 (line : col) \n \n callsFunction(2, 3, fn(x, y) x + y; }); \n                             " (util/yellow "^")))
           (with-out-str (parser/print-error (try (parser/run "callsFunction(2, 3, fn(x, y) x + y; });") (catch clojure.lang.ExceptionInfo e e)))))))
  (testing "null literals"
    (is (= "null;\n" (parse "null;")))
    (is (= "let x = null;\n" (parse "let x = null;"))))
  (testing "assign expression"
    (is (= "x = 5;\n" (parse "x = 5;")))
    (is (= "(x[0]) = 5;\n" (parse "x[0] = 5;")))
    (is (= "([1, 2][0]) = 5;\n" (parse "[1, 2][0] = 5;")))
    (is (= "({\"a\": 1}[\"a\"]) = 5;\n" (parse "{\"a\": 1}[\"a\"] = 5;")))))
