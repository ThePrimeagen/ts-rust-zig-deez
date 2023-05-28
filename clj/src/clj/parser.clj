(ns clj.parser
  (:require [clj.token :as token]
            [clj.ast   :as ast]
            [clj.lexer :as lexer]))

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
    :eq      EQUALS
    :noteq   EQUALS
    :lt      LTGT
    :gt      LTGT
    :gteq    LTGT
    :lteq    LTGT
    :plus    SUM
    :minus   SUM
    :slash   PRODUCT
    :astrisk PRODUCT
             LOWEST))

(defn chomp-semicolon [lexed]
  (drop-while #(= :semicolon (token/kind %)) lexed))

(defn parse-ident [[ident & rst :as lexed]]
  [(ast/ident (token/literal ident)) rst])

(defn parse-int [[int & rst :as lexed]]
  [(ast/int (Integer/parseInt (token/literal int))) rst])

(defn parse-prefix [[prefix & rst :as lexed]]
  (when-let [[right-expr rst] (parse-expr PREFIX (next lexed))]
    [(ast/prefix (token/literal prefix) right-expr) rst]))

(defn parse-infix [left-expr [infix & rst :as lexed]]
  (when-let [[right-expr rst] (parse-expr (precedence (token/kind infix)) rst)]
    [(ast/infix left-expr (token/literal infix) right-expr) rst]))

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
  ([prece [expr :as lexed]]
    (when-let [prefix    (prefix-parse-fn (token/kind expr))]
    (when-let [left-expr (prefix lexed)]
      (apply parse-expr prece left-expr))))
  ([prece left-expr [op :as rst]]
    (if-not (and (not= :semicolon (token/kind op))
                 (< prece (precedence (token/kind op))))  [left-expr rst]
    (if-let   [infix     (infix-parse-fn (token/kind op))]
    (when-let [left-expr (infix left-expr rst)]
      (apply parse-expr prece left-expr))                 [left-expr rst]))))

(defn parse-let [[ident op & rst :as lexed]]
  (when (= :assign (token/kind op))
  (when-let [[val rst] (parse-expr LOWEST rst)]
    [(ast/let (token/literal ident) val) (chomp-semicolon rst)])))

(defn parse-expr-stmt [lexed]
  (when-let [[expr rst] (parse-expr LOWEST lexed)]
    [(ast/expr expr) (chomp-semicolon rst)]))

(defn parse-stmt [[cur & rst :as lexed]]
  (case (token/kind cur)
    :let    (parse-let rst)
    :return nil
    :if     nil
            (parse-expr-stmt lexed)))

(defn parse-program [[cur :as lexed]]
  (when-not (= :eof (token/kind cur))
  (when-let [[stmt rst] (parse-stmt lexed)]
    (cons stmt (lazy-seq (parse-program rst))))))

(comment
  (parse-program (lexer/lex "foobar;"))
  (parse-program (lexer/lex "5;"))
  (parse-program (lexer/lex "!5;"))
  (parse-program (lexer/lex "-15;"))
  (parse-program (lexer/lex "5 + 5;"))
  (parse-program (lexer/lex "5 != 5;"))
  (parse-expr LOWEST (lexer/lex "5 != 5;"))
  (parse-program (lexer/lex "let hello = 5;"))
  (clojure.pprint/pprint(parse-program (lexer/lex "3 + 4 * 5 == 3 * 1 + 4 * 5")))
  ())
