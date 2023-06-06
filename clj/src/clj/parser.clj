(ns clj.parser
  (:require [clj.token :as token]
            [clj.ast   :as ast]
            [clj.lexer :as lexer]))

(defmacro return [parsed rest-tokens]
  `(list ~parsed ~rest-tokens))

(declare parse-expr parse-block-stmt parse-stmt)

(defn expect-peek [[cur :as tokens] expected-kind]
  (when (= expected-kind (token/kind cur))
    (rest tokens)))

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
     :l_paren  CALL
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

(defn parse-bool [[bool :as tokens]]
  (return (ast/bool (= :true (token/kind bool))) (rest tokens)))

(defn parse-group [tokens]
  (when-let [[expr rest-tokens] (parse-expr LOWEST (rest tokens))]
  (when-let [rest-tokens        (expect-peek rest-tokens :r_paren)]
    (return expr rest-tokens))))

(defn parse-if [tokens]
  (when-let [rest-tokens         (expect-peek (rest tokens) :l_paren)]
  (when-let [[condi rest-tokens] (parse-expr LOWEST rest-tokens)]
  (when-let [rest-tokens         (expect-peek rest-tokens :r_paren)]
  (when-let [rest-tokens         (expect-peek rest-tokens :l_squirly)]
  (when-let [[conse rest-tokens] (parse-block-stmt rest-tokens)]
  (when-let [rest-tokens         (expect-peek rest-tokens :r_squirly)]
  (if-not   (= :else (token/kind (token/next rest-tokens)))
    (return (ast/if condi conse nil) rest-tokens)
  (when-let [rest-tokens         (expect-peek (rest rest-tokens) :l_squirly)]
  (when-let [[alter rest-tokens] (parse-block-stmt rest-tokens)]
  (when-let [rest-tokens         (expect-peek rest-tokens :r_squirly)]
    (return (ast/if condi conse alter) rest-tokens))))))))))))

(defn parse-fn-params 
  ([tokens]
    (if (= :r_paren (token/kind (token/next tokens)))
      (return [] tokens)
    (when-let [[ident rest-tokens] (parse-ident tokens)]
      (parse-fn-params rest-tokens [ident]))))
  ([tokens idents]
    (let [rest-tokens (expect-peek tokens :comma)]
    (if-not rest-tokens
      (return idents tokens)
    (when-let [[ident rest-tokens] (parse-ident rest-tokens)]
      (recur rest-tokens (conj idents ident)))))))

(defn parse-fn [tokens]
  (when-let [rest-tokens          (expect-peek (rest tokens) :l_paren)]
  (when-let [[params rest-tokens] (parse-fn-params rest-tokens)]
  (when-let [rest-tokens          (expect-peek rest-tokens :r_paren)]
  (when-let [rest-tokens          (expect-peek rest-tokens :l_squirly)]
  (when-let [[block rest-tokens]  (parse-block-stmt rest-tokens)]
  (when-let [rest-tokens          (expect-peek rest-tokens :r_squirly)]
    (return (ast/fn params block) rest-tokens))))))))

(defn parse-call-args 
  ([tokens]
    (if (= :r_paren (token/kind (token/next tokens)))
      (return [] tokens)
    (when-let [[expr rest-tokens] (parse-expr LOWEST tokens)]
      (parse-call-args rest-tokens [expr]))))
  ([tokens exprs]
    (let [rest-tokens (expect-peek tokens :comma)]
    (if-not rest-tokens
      (return exprs tokens)
    (when-let [[expr rest-tokens] (parse-expr LOWEST rest-tokens)]
      (recur rest-tokens (conj exprs expr)))))))

#_{:clj-kondo/ignore [:unused-binding]}
(defn parse-call [fn-expr [lparan :as tokens]]
  (when-let [[args rest-tokens] (parse-call-args (rest tokens))]
  (when-let [rest-tokens        (expect-peek rest-tokens :r_paren)]
    (return (ast/call fn-expr args) rest-tokens))))

(defn prefix-parse-fn [token-type]
  (case token-type
     :ident   parse-ident
     :int     parse-int
     :l_paren parse-group
     :if      parse-if
     :fn      parse-fn
    (:bang  
     :minus)  parse-prefix
    (:true  
     :false)  parse-bool
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
     :l_paren  parse-call
               nil))

(defn parse-expr 
  ([prece [expr :as tokens]]
    (when-let [prefix    (prefix-parse-fn (token/kind expr))]
    (when-let [left-expr (prefix tokens)] ;; left-expr = [left-expr rest-tokens]
      (apply parse-expr prece left-expr))))
  ([prece left-expr [op :as rest-tokens]]
    (if (or (= :semicolon (token/kind op))
            (>= prece (precedence (token/kind op))))     (return left-expr rest-tokens)
    (let [infix (infix-parse-fn (token/kind op))]
    (if-not infix                                        (return left-expr rest-tokens)
    (when-let [left-expr (infix left-expr rest-tokens)]  (apply parse-expr prece left-expr)))))))

(defn parse-let [[ident :as tokens]]
  (when-let [rest-tokens       (expect-peek tokens :ident)]
  (when-let [rest-tokens       (expect-peek rest-tokens :assign)]
  (when-let [[val rest-tokens] (parse-expr LOWEST rest-tokens)]
    (return (ast/let (token/literal ident) val) 
            (chomp-semicolon rest-tokens))))))

(defn parse-return [tokens]
  (when-let [[expr rest-tokens] (parse-expr LOWEST tokens)]
    (return (ast/return expr) (chomp-semicolon rest-tokens))))

(defn parse-expr-stmt [tokens]
  (when-let [[expr rest-tokens] (parse-expr LOWEST tokens)]
    (return (ast/expr expr) (chomp-semicolon rest-tokens))))

(defn parse-block-stmt
  ([tokens]
    (parse-block-stmt tokens []))
  ([[cur :as rest-tokens] stmts]
    (if (or (= :r_squirly (token/kind cur))
            (= :eof       (token/kind cur)))
      (return (ast/block stmts) rest-tokens)
    (when-let [[stmt rest-tokens] (parse-stmt rest-tokens)]
      (recur rest-tokens (conj stmts stmt))))))

(defn parse-stmt [[cur :as tokens]]
  (case (token/kind cur)
    :let    (parse-let (rest tokens))
    :return (parse-return (rest tokens))
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
  (parse-program (lexer/lex "true;"))
  (parse-program (lexer/lex "let barfoo = false;"))
  (parse-program (lexer/lex "3 < 5 == true"))
  (parse-program (lexer/lex "!true;"))
  (parse-program (lexer/lex "2 / (5 + 5)"))
  (parse-program (lexer/lex "!(true == true)"))
  (parse-program (lexer/lex "1 + (2 + 3) + 4"))
  (parse-program (lexer/lex "if (x < y) { x }"))
  (parse-program (lexer/lex "if (x < y) { x } else { y }"))
  (parse-program (lexer/lex "if (x < y) { x; 1; } else { y; 0; }"))
  (parse-program (lexer/lex "fn(x, y) { x + y; };"))
  (parse-program (lexer/lex "fn(x, y) { x + y; y + 1 + x };"))
  (parse-program (lexer/lex "fn() {};"))
  (parse-program (lexer/lex "fn(x, y) {};"))
  (parse-program (lexer/lex "fn(x, y) { x + y; }(2, 3)"))
  (parse-program (lexer/lex "callsFunction(2, 3, fn(x, y) { x + y; });"))
  (parse-program (lexer/lex "add(1, 2 * 3, 4 + 5);"))
  (parse-program (lexer/lex "if (x < y) { return 5 } else { return 7 };"))
  (parse-program (lexer/lex "if (x < y) { return 5 };"))
  (parse-program (lexer/lex "3 + 4 * 5 == 3 * 1 + 4 * 5"))
  ())

