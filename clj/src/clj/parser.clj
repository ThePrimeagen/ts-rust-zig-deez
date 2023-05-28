(ns clj.parser
  (:require [clj.token :as token]
            [clj.ast   :as ast]
            [clj.lexer :as lexer]))

(defn return [parsed rest-tokens]
  [parsed rest-tokens])

(declare parse-expr)

;; precedence
(def ^:const LOWEST  0)
(def ^:const EQUALS  1)
(def ^:const LTGT    2)
(def ^:const SUM     3)
(def ^:const PRODUCT 4)
(def ^:const PREFIX  5)
(def ^:const CALL    6)

(defn precedence [token-type]
  (case token-type
    (:eq
     :noteq)   EQUALS
    (:lt
     :gt
     :gteq
     :lteq)    LTGT
    (:plus
     :minus)   SUM
    (:slash
     :astrisk) PRODUCT
               LOWEST))

(defn chomp-semicolon [tokens]
  (drop-while #(= :semicolon (token/kind %)) tokens))

(defn parse-ident [[ident :as tokens]]
  (return (ast/ident (token/literal ident)) (rest tokens)))

(defn parse-int [[int :as tokens]]
  (return (ast/int (Integer/parseInt (token/literal int))) (rest tokens)))

(defn parse-prefix [[prefix :as tokens]]
  (when-let [[right-expr rest-tokens] (parse-expr PREFIX (rest tokens))]
    (return (ast/prefix (token/literal prefix) right-expr) rest-tokens)))

(defn parse-infix [left-expr [infix :as tokens]]
  (when-let [[right-expr rest-tokens] (parse-expr (precedence (token/kind infix)) (rest tokens))]
    (return (ast/infix left-expr (token/literal infix) right-expr) rest-tokens)))

(defn prefix-parse-fn [token-type]
  (case token-type
    :ident parse-ident
    :int   parse-int
    :bang  parse-prefix
    :minus parse-prefix
           nil))

(defn infix-parse-fn [token-type]
  (case token-type
    (:eq
     :noteq
     :lt
     :gt
     :gteq
     :lteq
     :plus
     :minus
     :slash
     :astrisk) parse-infix 
               nil))

(defn parse-expr 
  ([prece [expr :as tokens]]
    (when-let [prefix    (prefix-parse-fn (token/kind expr))]
    (when-let [left-expr (prefix tokens)] ;; left-expr = [left-expr rest-tokens]
      (apply parse-expr prece left-expr))))
  ([prece left-expr [op :as rest-tokens]]
    (if-not (and (not= :semicolon (token/kind op))
                 (< prece (precedence (token/kind op))))  (return left-expr rest-tokens)
    (if-let   [infix     (infix-parse-fn (token/kind op))]
    (when-let [left-expr (infix left-expr rest-tokens)]
      (apply parse-expr prece left-expr))                 (return left-expr rest-tokens)))))

(defn parse-let [[ident op & rest-tokens :as tokens]]
  (when (= :assign (token/kind op))
  (when-let [[val rest-tokens] (parse-expr LOWEST rest-tokens)]
    (return (ast/let (token/literal ident) val) (chomp-semicolon rest-tokens)))))

(defn parse-expr-stmt [tokens]
  (when-let [[expr rest-tokens] (parse-expr LOWEST tokens)]
    (return (ast/expr expr) (chomp-semicolon rest-tokens))))

(defn parse-stmt [[cur :as tokens]]
  (case (token/kind cur)
    :let    (parse-let (rest tokens))
    :return nil
    :if     nil
            (parse-expr-stmt tokens)))

(defn parse-program [[cur :as tokens]]
  (when-not (= :eof (token/kind cur))
  (when-let [[stmt rest-tokens] (parse-stmt tokens)]
    (cons stmt (lazy-seq (parse-program rest-tokens))))))

(comment
  (parse-program (lexer/lex "foobar;"))
  (parse-program (lexer/lex "5;"))
  (parse-program (lexer/lex "!5;"))
  (parse-program (lexer/lex "-15;"))
  (parse-program (lexer/lex "5 + 5;"))
  (parse-program (lexer/lex "5 != 5;"))
  (parse-expr LOWEST (lexer/lex "5 != 5;"))
  (parse-expr LOWEST (lexer/lex "5 !=;"))
  (parse-program (lexer/lex "let hello = 5;"))
  (clojure.pprint/pprint(parse-program (lexer/lex "3 + 4 * 5 == 3 * 1 + 4 * 5")))
  ())
