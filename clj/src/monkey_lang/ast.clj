(ns monkey-lang.ast
  (:refer-clojure :exclude [if int fn])
  (:require [monkey-lang.util :refer [third fourth]]
            [clojure.string :as str]))

;; statements
(def kind first)

(defmacro let- [ident value]
  `(vector :let ~ident ~value))

(def let-ident second)
(def let-value third)


(defmacro expr [expr]
  `(vector :expr ~expr))

(def expr-expr second)


(defmacro block [stmts]
  `(vector :block ~stmts))

(def block-stmts second)


(defmacro return [expr]
  `(vector :return ~expr))

(def return-expr second)


;; expressions
(defmacro prefix [op right]
  `(vector :prefix ~op ~right))

(def prefix-op    second)
(def prefix-right third)


(defmacro infix [left op right]
  `(vector :infix ~left ~op ~right))

(def infix-left  second)
(def infix-op    third)
(def infix-right fourth)


(defmacro if [condition consequence alternative]
  `(vector :if ~condition ~consequence ~alternative))

(def if-condition   second)
(def if-consequence third)
(def if-alternative fourth)


(defmacro fn [params block]
  `(vector :fn ~params ~block))

(def fn-params second)
(def fn-block  third)


(defmacro call [fn args]
  `(vector :call ~fn ~args))

(def call-fn   second)
(def call-args third)


(defmacro index-expr [left index]
  `(vector :index-expr ~left ~index))

(def index-expr-left  second)
(def index-expr-index third)


;; Literals
(defmacro int [val]
  `(vector :int ~val))

(def int-value second)


(defmacro bool [val]
  `(vector :bool ~val))

(def bool-value second)


(defmacro ident [literal]
  `(vector :ident ~literal))

(def ident-literal second)


(defmacro string [literal]
  `(vector :string ~literal))

(def string-value second)


(defmacro array [elements]
  `(vector :array ~elements))

(def array-elements second)


(defmacro hash [pairs]
  `(vector :hash ~pairs))

(def hash-pairs second)


(defmacro program [stmts]
  `(vector :program ~stmts))

(def program-stmts second)

;; TODO: cleanup this function a little bit
(defn to-str [ast]
  (case (kind ast)
    :program (str/join (mapv to-str (program-stmts ast)))
    ;; statements
    :let    (str "let " (let-ident ast) " = " (to-str (let-value ast)) ";" \newline)
    :expr   (str (to-str (expr-expr ast)) ";" \newline)
    :block  (if (empty? (block-stmts ast))
              (str "{ }")  
            (str "{" \newline (str/join (mapv (comp #(str \space \space %) to-str) (block-stmts ast))) "}"))
    :return (str "return " (to-str (return-expr ast)) ";" \newline)
    ;; expressions
    :prefix (str "(" (prefix-op ast) (to-str (prefix-right ast)) ")")
    :infix  (str "(" (to-str (infix-left ast)) \space (infix-op ast) \space (to-str (infix-right ast)) ")")
    :if     (str "if" \space "(" (to-str (if-condition ast)) ")" \space
                  (to-str (if-consequence ast))
                  (when-not (empty? (if-alternative ast))
                    (str " else " (to-str (if-alternative ast)))))
    :fn     (str "fn (" (str/join ", " (mapv to-str (fn-params ast))) ")" \space
                 (to-str (fn-block ast)))
    :call   (str (to-str (call-fn ast)) "(" (str/join ", " (mapv to-str (call-args ast))) ")")
    :index-expr (str "(" (to-str (index-expr-left ast)) "[" (to-str (index-expr-left ast)) "])")
    ;; literals
    :ident  (str (ident-literal ast))
    :int    (str (int-value ast))
    :bool   (str (bool-value ast))
    :string (str \" (string-value ast) \")
    :array  (str "[" (str/join ", "(mapv to-str (array-elements ast))) "]")
    :hash   (let [pairs (for [pair (hash-pairs ast)]
                          (str/join ": " (mapv to-str pair)))]
            (str "{" (str/join ", " pairs) "}"))
    (assert ast (str "ast/to-str not implemented for " ast))))

(def pprint (comp println to-str))
