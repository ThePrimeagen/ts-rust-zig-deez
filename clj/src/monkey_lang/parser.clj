(ns monkey-lang.parser
  (:require [jdsl.combinator   :as jc]
            [jdsl.basic        :as jb]
            [jdsl.char-stream  :as cs]
            [jdsl.char-parser  :as jp]
            [monkey-lang.token :as token]
            [monkey-lang.ast   :as ast]
            [monkey-lang.lexer :as lexer]
            [clojure.string    :as str]
            [monkey-lang.util  :as util]))

(declare parse-expr parse-block-stmts parse-stmt)

;; precedence
(def ^:const PREC_LOWEST  0)
(def ^:const PREC_ASSIGN  1)
(def ^:const PREC_EQUALS  2)
(def ^:const PREC_LTGT    3)
(def ^:const PREC_SUM     4)
(def ^:const PREC_PRODUCT 5)
(def ^:const PREC_PREFIX  6)
(def ^:const PREC_CALL    7)
(def ^:const PREC_INDEX   8)

(defn precedence [token]
  (case (token/kind token)
     :token/assign    PREC_ASSIGN
    (:token/eq
     :token/noteq)    PREC_EQUALS
    (:token/lt
     :token/gt
     :token/gteq
     :token/lteq)     PREC_LTGT
    (:token/plus
     :token/minus)    PREC_SUM
    (:token/slash
     :token/astrisk)  PREC_PRODUCT
     :token/l-paren   PREC_CALL
     :token/l-bracket PREC_INDEX
     :token/dot       PREC_INDEX
                      PREC_LOWEST))

(def parse-ident
  (jb/do
    (ident <- (lexer/expect token/IDENT))
    (jc/return (ast/ident (token/literal ident)))))

(def parse-int
  (jb/do
    (integer <- (lexer/expect token/INT))
    (let [parsed-int (Integer/parseInt (token/literal integer))]
    (jc/return (ast/int parsed-int)))))

(def parse-null
  (jb/do
    (lexer/expect token/NULL)
    (jc/return ast/null)))

(def parse-prefix
  (jb/do
    (prefix     <- (-> lexer/next (jc/label "Expected: prefix function")))
    (right-expr <- (parse-expr PREC_PREFIX))
    (jc/return (ast/prefix (token/literal prefix) right-expr))))

(defn parse-infix [lexpr]
  (jb/do
    (infix      <- (-> lexer/next (jc/label "Expected: infix function")))
    (right-expr <- (parse-expr (precedence infix)))
    (jc/return (ast/infix lexpr (token/literal infix) right-expr))))

(def parse-bool
  (jb/do
    (bool <- (-> (lexer/expect token/TRUE) (jc/<|> (lexer/expect token/FALSE))))
    (jc/return (ast/bool (token/is? bool token/TRUE)))))

(def parse-group
  (jb/do
    (lexer/expect token/LPAREN)
    (expr <- (parse-expr PREC_LOWEST))
    (lexer/expect token/RPAREN)
    (jc/return expr)))

(def parse-if
  (jb/do
    (lexer/expect token/IF)
    (lexer/expect token/LPAREN)
    (condi <- (parse-expr PREC_LOWEST)) ;; condition
    (lexer/expect token/RPAREN)
    (conse <- parse-block-stmts) ;; consequence
    (token <- lexer/peek)
    (if-not (token/is? token token/ELSE)
      (jc/return (ast/if condi conse nil))
    (jb/do
      (lexer/expect token/ELSE)
      (altrn <- parse-block-stmts) ;; alternative
      (jc/return (ast/if condi conse altrn))))))

(def parse-fn-params
  (jb/do
    (lexer/expect token/LPAREN)
    (params <- (-> parse-ident (jc/sep-by* (lexer/expect token/COMMA))))
    (lexer/expect token/RPAREN)
    (jc/return params)))

(def parse-fn
  (jb/do
    (lexer/expect token/FUNCTION)
    (params <- parse-fn-params)
    (block  <- parse-block-stmts)
    (jc/return (ast/fn params block))))

(defn parse-fn-call [fn-expr]
  (jb/do
    (lexer/expect token/LPAREN)
    (exprs <- (-> (parse-expr PREC_LOWEST) (jc/sep-by* (lexer/expect token/COMMA))))
    (lexer/expect token/RPAREN)
    (jc/return (ast/call fn-expr exprs))))

(def parse-string
  (jb/do
    (string <- (lexer/expect token/STRING))
    (jc/return (ast/string (token/literal string)))))

(def parse-array
  (jb/do
    (lexer/expect token/LBRACKET)
    (elements <- (-> (parse-expr PREC_LOWEST) (jc/sep-by* (lexer/expect token/COMMA))))
    (lexer/expect token/RBRACKET)
    (jc/return (ast/array elements))))

(defn parse-index-expr [lexpr]
  (jb/do
    (lexer/expect token/LBRACKET)
    (index <- (parse-expr PREC_LOWEST))
    (lexer/expect token/RBRACKET)
    (jc/return (ast/index-expr lexpr index))))

(def hash-pair
  (jb/do
    (k <- (parse-expr PREC_LOWEST))
    (lexer/expect token/COLON)
    (v <- (parse-expr PREC_LOWEST))
    (let [k (if (ast/is? k ast/IDENT_LIT) (ast/ident->string k) k)]
    (jc/return [k v]))))

(def parse-hash
  (jb/do
    (lexer/expect token/LBRACE)
    (pairs <- (-> hash-pair (jc/sep-by* (lexer/expect token/COMMA))))
    (lexer/expect token/RBRACE)
    (jc/return (ast/hash pairs))))

(defn parse-assign-expr [left]
  (jb/do
    (lexer/expect token/ASSIGN)
    (right <- (parse-expr PREC_LOWEST))
    (jc/return (ast/assign-expr left right))))

(defn parse-accessor-expr [left]
  (jb/do
    (lexer/expect token/DOT)
    (prop <- lexer/next)
    (jc/return (ast/index-expr left (ast/string (token/literal prop))))))

(def parse-while
  (jb/do
    (lexer/expect token/WHILE)
    (lexer/expect token/LPAREN)
    (condi <- (parse-expr PREC_LOWEST)) ;; condition
    (lexer/expect token/RPAREN)
    (conse <- parse-block-stmts) ;; consequence
    (jc/return (ast/while-expr condi conse))))

(defn prefix-parse-fn [token]
  (case (token/kind token)
     :token/ident   parse-ident
     :token/int     parse-int
     :token/l-paren parse-group
     :token/if      parse-if
     :token/fn      parse-fn
     :token/while   parse-while
     :token/string  parse-string
     :token/l-bracket parse-array
     :token/l-brace parse-hash
     :token/null    parse-null
    (:token/bang  
     :token/minus)  parse-prefix
    (:token/true  
     :token/false)  parse-bool
                    (jc/fail-with (jb/error "No expression prefix function for " token))))

(defn infix-parse-fn [token]
  (case (token/kind token)
    (:token/eq
     :token/noteq
     :token/lt
     :token/gt
     :token/gteq
     :token/lteq
     :token/plus
     :token/minus
     :token/slash
     :token/astrisk)  parse-infix
     :token/l-paren   parse-fn-call
     :token/l-bracket parse-index-expr
     :token/assign    parse-assign-expr
     :token/dot       parse-accessor-expr 
                      nil))

(defn parse-expr 
  ([prece]
    (jb/do
      (token <- lexer/peek)
      (lexpr <- (prefix-parse-fn token))
      (parse-expr prece lexpr)))
  ([prece lexpr]
    (jb/do
      (token <- lexer/peek)
      (if (or (token/is? token token/SEMICOLON)
              (>= prece (precedence token))
              (not (infix-parse-fn token)))
        (jc/return lexpr)
      (jb/do
        (lexpr <- ((infix-parse-fn token) lexpr))
        (parse-expr prece lexpr))))))

(def parse-let-stmt
  (jb/do
    (lexer/expect token/LET)
    (ident <- (lexer/expect token/IDENT))
    (lexer/expect token/ASSIGN)
    (value <- (parse-expr PREC_LOWEST))
    (lexer/expect token/SEMICOLON)
    (jc/return (ast/let- (token/literal ident) value))))

(def parse-return-stmt
  (jb/do
    (lexer/expect token/RETURN)
    (expr <- (parse-expr PREC_LOWEST))
    (lexer/expect token/SEMICOLON)
    (jc/return (ast/return expr))))

(def parse-expr-stmt 
  (jb/do
    (expr <- (parse-expr PREC_LOWEST))
    (jc/skip (lexer/expect token/SEMICOLON))
    (jc/return (ast/expr expr))))

(defn replace-tail-call [stmts]
  (when-not (empty? stmts)
    (let [last-stmt (peek stmts)
          ret-expr  (ast/return-expr last-stmt)]
    (if (and (ast/is? last-stmt ast/RETURN_STMT)
             (ast/is? ret-expr  ast/CALL_EXPR))
      (conj (pop stmts) 
            (ast/tail-call (ast/call-fn ret-expr) (ast/call-args ret-expr)))
    (-> stmts)))))

(def parse-break-stmt
  (jb/do
    (lexer/expect token/BREAK)
    (jc/skip (lexer/expect token/SEMICOLON))
    (jc/return ast/break)))

(def parse-continue-stmt
  (jb/do
    (lexer/expect token/CONTINUE)
    (jc/skip (lexer/expect token/SEMICOLON))
    (jc/return ast/continue)))

(def parse-block-stmts
  (jb/do
    (lexer/expect token/LBRACE)
    (stmts <- (jc/many* parse-stmt))
    (lexer/expect token/RBRACE)
    (jc/return (ast/block (replace-tail-call stmts)))))

(def parse-stmt
  (jb/do
    (token <- lexer/peek)
    (case (token/kind token)
      :token/let    parse-let-stmt
      :token/return parse-return-stmt
      :token/break  parse-break-stmt
      :token/continue parse-continue-stmt
                    parse-expr-stmt)))

(def parse-program
  (jb/do
    (stmts <- (jc/many+ parse-stmt))
    (lexer/expect token/EOF)
    (jc/return (ast/program (ast/block stmts)))))

(defn print-error [e]
  (let [ex-data (ex-data e)
        ex-msg  (ex-message e)
        [_ ts]  (jb/run jp/skip-spaces* (:ts ex-data))
        [found] (jb/run lexer/peek ts)
        program (util/to-str (cs/buf ts))
        pos     (cs/pos ts)
        lines   (str/split-lines (subs program 0 (inc pos)))
        lyn-num (count lines)
        col-num (count (last lines))
        err-lyn (nth (str/split-lines program) (dec lyn-num) 0)
        padding (util/to-str (repeat (dec col-num) \space))
        pointer (util/yellow "^")]
  (println (util/yellow ex-msg) \newline 
           (or (:msg ex-data) "Expected: ") \newline
           "   Found:" found \newline
           "Location:" lyn-num ":" col-num "(line : col)" \newline \newline
           err-lyn \newline
           padding pointer)))

(defn run [input]
  (jb/parsed (jb/run parse-program (cs/create input))))

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
  (run "a[1]")
  (println (ast/to-str (run "3 + 4 * 5 == 3 * 1 + 4 * 5")))
  (ast/to-str (run "{\"one\": 1, \"two\": 2, \"three\": 3}"))
  (run "let sumTo = fn (n, acc) { if (n == 0) { return acc; }; return sumTo(n-1, acc+n); }; sumTo(10000, 0);"))
