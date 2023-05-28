(ns clj.ast
  (:refer-clojure :exclude [let if int])
  (:use [clj.util :only [third]]))

  ;; statements
(def stmt-kind first)

(defn let [ident value]
  [:let ident value])

(def let-ident second)
(def let-value third)


(defn expr [expr]
  [:expr expr])

(def expr-expr second)


;; expressions
(defn ident [literal]
  [:ident literal])

(def ident-literal second)


(defn int [val]
  [:int val])

(def int-value second)


(defn prefix [op right]
  [:prefix op right])

(def prefix-op    first)
(def prefix-right second)


(defn infix [left op right]
  [:infix left op right])

(def infix-left  first)
(def infix-op    second)
(def infix-right third)
