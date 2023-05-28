(ns clj.ast
  (:refer-clojure :exclude [let if int])
  (:use [clj.util :only [third]]))

  ;; statements
(def stmt-kind first)

(defmacro let [ident value]
  `(list :let ~ident ~value))

(def let-ident second)
(def let-value third)


(defmacro expr [expr]
  `(list :expr ~expr))

(def expr-expr second)


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
