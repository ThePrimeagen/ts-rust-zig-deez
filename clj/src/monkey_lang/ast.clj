(ns monkey-lang.ast
  (:refer-clojure :exclude [if int fn hash])
  (:require [monkey-lang.util :refer [third fourth pad]]
            [clojure.string :as str]))

(def ^:const PROGRAM     :ast/program)
(def ^:const LET_STMT    :ast/let-stmt)
(def ^:const EXPR_STMT   :ast/expr-stmt)
(def ^:const BLOCK_STMT  :ast/block-stmt)
(def ^:const RETURN_STMT :ast/return-stmt)
(def ^:const PREFIX_EXPR :ast/prefix-expr)
(def ^:const INFIX_EXPR  :ast/infix-expr)
(def ^:const IF_EXPR     :ast/if-expr)
(def ^:const CALL_EXPR   :ast/call-expr)
(def ^:const TAIL_CALL   :ast/tail-call)
(def ^:const INDEX_EXPR  :ast/index-expr)
(def ^:const ASSIGN_EXPR :ast/assign-expr)
(def ^:const FN_LIT      :ast/fn-lit)
(def ^:const INT_LIT     :ast/int-lit)
(def ^:const BOOL_LIT    :ast/bool-lit)
(def ^:const IDENT_LIT   :ast/ident-lit)
(def ^:const STRING_LIT  :ast/string-lit)
(def ^:const ARRAY_LIT   :ast/array-lit)
(def ^:const HASH_LIT    :ast/hash-lit)
(def ^:const NULL_LIT    :ast/null-lit)

;; statements
(def kind first)

(defmacro let- [ident value]
  `(vector ~LET_STMT ~ident ~value))

(def let-ident second)
(def let-value third)


(defmacro expr [expr]
  `(vector ~EXPR_STMT ~expr))

(def expr-expr second)


(defmacro block [stmts]
  `(vector ~BLOCK_STMT ~stmts))

(def block-stmts second)


(defmacro return [expr]
  `(vector ~RETURN_STMT ~expr))

(def return-expr second)


;; expressions
(defmacro prefix [op right]
  `(vector ~PREFIX_EXPR ~op ~right))

(def prefix-op    second)
(def prefix-right third)


(defmacro infix [left op right]
  `(vector ~INFIX_EXPR ~left ~op ~right))

(def infix-left  second)
(def infix-op    third)
(def infix-right fourth)


(defmacro if [condition consequence alternative]
  `(vector ~IF_EXPR ~condition ~consequence ~alternative))

(def if-condition   second)
(def if-consequence third)
(def if-alternative fourth)


(defmacro fn [params block]
  `(vector ~FN_LIT ~params ~block))

(def fn-params second)
(def fn-block  third)


(defmacro call [fn args]
  `(vector ~CALL_EXPR ~fn ~args))

(def call-fn   second)
(def call-args third)

(defmacro tail-call [fn args]
  `(vector ~TAIL_CALL ~fn ~args))

(def tail-call-fn   second)
(def tail-call-args third)


(defmacro index-expr [left index]
  `(vector ~INDEX_EXPR ~left ~index))

(def index-expr-left  second)
(def index-expr-index third)

(defmacro assign-expr [left right]
  `(vector ~ASSIGN_EXPR ~left ~right))

(def assign-expr-left  second)
(def assign-expr-right third)


;; Literals
(defmacro int [val]
  `(vector ~INT_LIT ~val))

(def int-value second)


(defmacro bool [val]
  `(vector ~BOOL_LIT ~val))

(def bool-value second)


(defmacro ident [literal]
  `(vector ~IDENT_LIT ~literal))

(def ident-literal second)


(defmacro string [literal]
  `(vector ~STRING_LIT ~literal))

(def string-value second)


(defmacro array [elements]
  `(vector ~ARRAY_LIT ~elements))

(def array-elements second)


(defmacro hash [pairs]
  `(vector ~HASH_LIT ~pairs))

(def hash-pairs second)

(def null (vector NULL_LIT nil))

(defmacro program [stmts]
  `(vector ~PROGRAM ~stmts))

(def program-stmts second)

(defmacro is? [ast kynd]
  `(= ~kynd (kind ~ast)))

(defn to-str 
  ([ast]
    (to-str 0 ast))
  ([pad-lvl ast]
    (case (kind ast)
      :ast/program    (str/join (mapv (partial to-str pad-lvl) (program-stmts (block-stmts ast))))
      ;; statement
      :ast/let-stmt   (str (pad pad-lvl)  "let " (let-ident ast) " = " (to-str pad-lvl (let-value ast)) ";" \newline)
      :ast/expr-stmt  (str (pad pad-lvl) (to-str pad-lvl (expr-expr ast)) ";" \newline)
      :ast/block-stmt (if (empty? (block-stmts ast))
                        (str "{ }")
                      (let [stmts (mapv (partial to-str (inc pad-lvl)) (block-stmts ast))]
                      (str "{" \newline (str/join stmts) (pad pad-lvl) "}")))
      
      :ast/return-stmt (str (pad pad-lvl) "return " (to-str pad-lvl (return-expr ast)) ";" \newline)
      ;; expressions
      :ast/prefix-expr (str "(" (prefix-op ast) (to-str (prefix-right ast)) ")")
      :ast/infix-expr (str "(" (to-str (infix-left ast)) " " (infix-op ast) " " (to-str (infix-right ast)) ")")
      :ast/if-expr    (let [condi (to-str (if-condition ast))
                            conse (to-str pad-lvl (if-consequence ast))
                            altrn (when-not (empty? (block-stmts (if-alternative ast)))
                                    (str " else " (to-str pad-lvl (if-alternative ast))))]
                      (str "if (" condi ") " conse altrn))
      
      :ast/fn-lit     (let [params (mapv to-str (fn-params ast))
                            block  (to-str pad-lvl (fn-block ast))]
                      (str "fn (" (str/join ", " params) ") " block))
      
      :ast/call-expr  (str (to-str (call-fn ast)) "(" (str/join ", " (mapv to-str (call-args ast))) ")")
      :ast/tail-call  (str "return " (to-str (tail-call-fn ast)) "(" (str/join ", " (mapv to-str (tail-call-args ast))) ");" \newline)
      :ast/index-expr (str "(" (to-str (index-expr-left ast)) "[" (to-str (index-expr-index ast)) "])")
      :ast/assign-expr (str (to-str (assign-expr-left ast)) " = " (to-str (assign-expr-right ast)))
      ;; literals
      :ast/ident-lit  (ident-literal ast)
      :ast/int-lit    (str (int-value ast))
      :ast/bool-lit   (str (bool-value ast))
      :ast/string-lit (str \" (string-value ast) \")
      :ast/array-lit  (str "[" (str/join ", "(mapv to-str (array-elements ast))) "]")
      :ast/hash-lit   (let [pairs (for [pair (hash-pairs ast)]
                                    (str/join ": " (mapv to-str pair)))]
                      (str "{" (str/join ", " pairs) "}"))
      :ast/null-lit   "null"
      (assert ast (str "ast/to-str not implemented for " ast)))))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def pprint (comp println to-str))
