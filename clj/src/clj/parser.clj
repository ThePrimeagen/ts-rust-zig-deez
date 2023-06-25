(ns clj.parser
  {:clj-kondo/ignore [:unresolved-symbol]}
  (:require [jdsl.combinator  :as jc]
            [jdsl.basic       :as jb]
            [jdsl.char-stream :as cs]
            [jdsl.char-parser :as jp]
            [clj.token        :as token]
            [clj.ast          :as ast]
            [clj.lexer        :as lexer]
            [clojure.string   :as str]
            [clj.util         :as util]))

(declare parse-expr parse-block-stmts parse-stmt)

;; precedence
(def ^:const LOWEST  0)
(def ^:const EQUALS  1)
(def ^:const LTGT    2)
(def ^:const SUM     3)
(def ^:const PRODUCT 4)
(def ^:const PREFIX  5)
(def ^:const CALL    6)

(defn precedence [token]
  (case (token/kind token)
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

(def chomp-semicolon
  (jc/skip-many* (lexer/expect :semicolon)))

(def parse-ident
  (jb/do
    (ident <- (lexer/expect :ident))
    (jc/return (ast/ident (token/literal ident)))))

(def parse-int
  (jb/do
    (integer <- (lexer/expect :int))
    (let [parsed-int (Integer/parseInt (token/literal integer))]
    (jc/return (ast/int parsed-int)))))

(def parse-prefix
  (jb/do
    (prefix     <- (-> lexer/next (jc/label "Expected: prefix function")))
    (right-expr <- (parse-expr PREFIX))
    (jc/return (ast/prefix (token/literal prefix) right-expr))))

(defn parse-infix [lexpr]
  (jb/do
    (infix      <- (-> lexer/next (jc/label "Expected: infix function")))
    (right-expr <- (parse-expr (precedence infix)))
    (jc/return (ast/infix lexpr (token/literal infix) right-expr))))

(def parse-bool
  (jb/do
    (bool <- (-> (lexer/expect :true) (jc/<|> (lexer/expect :false))))
    (jc/return (ast/bool (= :true (token/kind bool))))))

(def parse-group
  (jb/do
    (lexer/expect :l_paren)
    (expr <- (parse-expr LOWEST))
    (lexer/expect :r_paren)
    (jc/return expr)))

(def parse-if
  (jb/do
    (lexer/expect :if)
    (lexer/expect :l_paren)
    (condi <- (parse-expr LOWEST)) ;; condition
    (lexer/expect :r_paren)
    (conse <- parse-block-stmts)   ;; consequence
    (token <- lexer/peek)
    (if-not (= :else (token/kind token))
      (jc/return (ast/if condi conse nil))
    (jb/do
      (lexer/expect :else)
      (altrn <- parse-block-stmts) ;; alternative
      (jc/return (ast/if condi conse altrn))))))

(def parse-fn-params
  (jb/do
    (lexer/expect :l_paren)
    (params <- (-> parse-ident (jc/sep-by* (lexer/expect :comma))))
    (lexer/expect :r_paren)
    (jc/return params)))

(def parse-fn
  (jb/do
    (lexer/expect :fn)
    (params <- parse-fn-params)
    (block  <- parse-block-stmts)
    (jc/return (ast/fn params block))))

(def parse-call-args
  (jb/do
    (lexer/expect :l_paren)
    (exprs <- (-> (parse-expr LOWEST) (jc/sep-by* (lexer/expect :comma))))
    (lexer/expect :r_paren)
    (jc/return exprs)))

(defn parse-fn-call [fn-expr]
  (jb/do
    (args <- parse-call-args)
    (jc/return (ast/call fn-expr args))))

(defn prefix-parse-fn [token]
  (case (token/kind token)
     :ident   parse-ident
     :int     parse-int
     :l_paren parse-group
     :if      parse-if
     :fn      parse-fn
    (:bang  
     :minus)  parse-prefix
    (:true  
     :false)  parse-bool
              (jc/fail-with (jb/error "No expression prefix function for " token))))

(defn infix-parse-fn [token]
  (case (token/kind token)
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
     :l_paren  parse-fn-call
               nil)) ;; coz infix-parsers expect on argument

(defn parse-expr 
  ([prece]
    (jb/do
      (token <- lexer/peek)
      (lexpr <- (prefix-parse-fn token))
      (parse-expr prece lexpr)))
  ([prece lexpr]
    (jb/do
      (token <- lexer/peek)
      (if (or (= :semicolon (token/kind token))
              (>= prece (precedence token))
              (not (infix-parse-fn token)))
        (jc/return lexpr)
      (jb/do
        (lexpr <- ((infix-parse-fn token) lexpr))
        (parse-expr prece lexpr))))))

(def parse-let-stmt
  (jb/do
    (lexer/expect :let)
    (ident <- (lexer/expect :ident))
    (lexer/expect :assign)
    (value <- (parse-expr LOWEST))
    (chomp-semicolon)
    (jc/return (ast/let (token/literal ident) value))))

(def parse-return-stmt
  (jb/do
    (lexer/expect :return)
    (expr <- (parse-expr LOWEST))
    (chomp-semicolon)
    (jc/return (ast/return expr))))

(def parse-expr-stmt 
  (jb/do
    (expr <- (parse-expr LOWEST))
    (chomp-semicolon)
    (jc/return (ast/expr expr))))

(def parse-block-stmts
  (jb/do
    (lexer/expect :l_squirly)
    (stmts <- (jc/many* parse-stmt))
    (lexer/expect :r_squirly)
    (jc/return (ast/block stmts))))

(def parse-stmt
  (jb/do
    (token <- lexer/peek)
    (case (token/kind token)
      :let    parse-let-stmt
      :return parse-return-stmt
              parse-expr-stmt)))

(def parse-program
  (jb/do
    (stmts <- (jc/many+ parse-stmt))
    (lexer/expect :eof)
    (jc/return (ast/program stmts))))

(defn print-error [e]
  (let [ex-data (ex-data e)
        ex-msg  (ex-message e)
        [_ ts]  (jb/run jp/skip-spaces* (:ts ex-data))
        [found] (jb/run lexer/peek ts)
        program (cs/string ts)
        pos     (cs/position ts)
        lines   (str/split-lines (subs program 0 (inc pos)))
        lyn-num (count lines)
        col-num (count (last lines))
        err-lyn (nth (str/split-lines program) (dec lyn-num) 0)
        padding (util/to-str (repeat (dec col-num) \space))
        pointer "^"]
  (println ex-msg \newline 
           (or (:msg ex-data) "Expected: ") \newline
           "   Found:" found \newline
           "Location:" lyn-num ":" col-num "(line : col)" \newline \newline
           err-lyn \newline
           padding pointer \newline)))

(defn run [input]
  (first (jb/run parse-program (cs/create input))))

(comment
  (println (ast/to-str (run "foobar; 5;")))
  (run "5;")
  (run "!5;")
  (run "-15;")
  (run "5 + 5;")
  (run "5 != 5;")
  (run "let hello | 5;")
  (run "true;")
  (run "let barfoo = false;")
  (jb/run parse-let-stmt (cs/create "let barfoo = false;"))
  (run "3 < 5 == true")
  (println (ast/to-str (run "!true;")))
  (run "2 / (5 + 5)")
  (run "!(true == true)")
  (run "1 + (2 + 3) + 4")
  (run "if (x < y) { x }")
  (run "if (x < y) { } else { }")
  (run "if (x < y) { x } else { y }")
  (run "if (x < y) { x; 1; }")
  (run "if (x < y) { x; 1; } else { y; 0; }")
  (println (ast/to-str (run "if (x < y) { x; 1; } else { y; 0; }")))
  (run "fn(x, y) { x + y; };")
  (run "fn(x, y) { x + y; y + 1 + x };")
  (println (ast/to-str (run "fn(x, y) { x + y; y + 1 + x };")))
  (run "fn() {};")
  (println (ast/to-str (run "fn(x, y) {};")))
  (run "fn(x, y) { x + y; }(2, 3)")
  (run "callsFunction(2, 3, fn(x, y) x + y; });")
  (println (ast/to-str (run "callsFunction(2, 3, fn(x, y) { x + y; });")))
  (run "add(1, 2 * 3, 4 + 5);")
  (run "if (x < y) { return 5 } else { return 7 };")
  (run "if (x < y) { return 5 };")
  (println (ast/to-str (run "3 + 4 * 5 == 3 * 1 + 4 * 5")))
  ())