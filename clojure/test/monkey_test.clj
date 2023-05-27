(ns monkey-test
  #_{:clj-kondo/ignore [:refer-all]}
  (:require [clojure.test :refer :all]
            [monkey :refer :all]
            [clojure.data]))

(def expected-tokens-let
  [{:token :let}
   {:token :ident, :literal "five"}
   {:token :equal}
   {:token :int, :literal 5}
   {:token :semicolon}
   {:token :eof}])

(deftest parse-let
  (let [[a b & _] (clojure.data/diff expected-tokens-let (tokenize "let five = 5;"))]
    (is (and (nil? a) (nil? b)))))

(def input-program "
let five = 5;
let ten = 10;
let add = fn(x, y) {
    x + y;
};
let result = add(five, ten);")

(def expected-tokens-program
  [{:token :let}
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
   {:token :eof}])

(deftest parse-monkey-program
  (let [[a b & _] (clojure.data/diff expected-tokens-program (tokenize input-program))]
    (is (and (nil? a) (nil? b)))))

