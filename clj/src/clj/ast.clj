(ns clj.ast
  (:refer-clojure :exclude [let if int fn])
  (:require [clj.util :refer [third]]))

  ;; statements
(def stmt-kind first)

(defmacro let [ident value]
  `(list :let ~ident ~value))

(def let-ident second)
(def let-value third)


(defmacro expr [expr]
  `(list :expr ~expr))

(def expr-expr second)


(defmacro block [stmts]
  `(list :block ~stmts))

(def block-stmts second)


(defmacro return [expr]
  `(list :return ~expr))

(def return-expr second)

;; expressions
(defmacro ident [literal]
  `(list :ident ~literal))

(def ident-literal second)


(defmacro int [val]
  `(list :int ~val))

(def int-value second)


(defmacro prefix [op right]
  `(list :prefix ~op ~right))

(def prefix-op    first)
(def prefix-right second)


(defmacro infix [left op right]
  `(list :infix ~left ~op ~right))

(def infix-left  first)
(def infix-op    second)
(def infix-right third)


(defmacro bool [val]
  `(list :bool ~val))

(def bool-value second)


(defmacro if [condition consequence alternative]
  `(list :if ~condition ~consequence ~alternative))

(def if-condition   first)
(def if-consequence second)
(def if-alternative third)


(defmacro fn [params block]
  `(list :fn ~params ~block))

(def fn-params first)
(def fn-block  second)


(defmacro call [fn args]
  `(list :call ~fn ~args))

(def call-fn   first)
(def call-args second)
