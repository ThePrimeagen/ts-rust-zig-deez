(ns monkey-test
  #_{:clj-kondo/ignore [:refer-all]}
  (:require [clojure.test :refer :all]
            [monkey :refer :all]))

(deftest parse-monkey
  (testing "let five = 5;"
    (let [result [{:token :let}
                  {:token :ident, :literal "five"}
                  {:token :equal}
                  {:token :int, :literal 5}
                  {:token :semicolon}
                  {:token :eof}]]
      (is (= result (parse "let five = 5;")))))
  (testing "complex program"
    (let [result [{:token :let}
                  {:token :ident, :literal "five"}
                  {:token :equal}
                  {:token :int, :literal 5}
                  {:token :semicolon}
                  {:token :let}
                  {:token :ident, :literal "ten"}
                  {:token :equal}
                  {:token :int, :literal 10}
                  {:token :semicolon}
                  {:token :let}
                  {:token :ident, :literal "add"}
                  {:token :equal}
                  {:token :function}
                  {:token :lparen}
                  {:token :ident, :literal "x"}
                  {:token :comma}
                  {:token :ident, :literal "y"}
                  {:token :rparen}
                  {:token :lbrace}
                  {:token :ident, :literal "x"}
                  {:token :plus}
                  {:token :ident, :literal "y"}
                  {:token :semicolon}
                  {:token :rbrace}
                  {:token :semicolon}
                  {:token :let}
                  {:token :ident, :literal "result"}
                  {:token :equal}
                  {:token :ident, :literal "add"}
                  {:token :lparen}
                  {:token :ident, :literal "five"}
                  {:token :comma}
                  {:token :ident, :literal "ten"}
                  {:token :rparen}
                  {:token :semicolon}
                  {:token :eof}]]
      (is (= result (parse (slurp "input.monkey")))))))
