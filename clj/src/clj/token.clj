(ns clj.token
  (:use [clj.util :only [third]]))

(defn chr->kind [chr]
  (case chr 
    \= :assign
    \+ :plus
    \, :comma
    \; :semicolon
    \( :l_paren
    \) :r_paren
    \{ :l_squirly
    \} :r_squirly
       nil))

(def chr-token? chr->kind)

(defn ident->kind [ident]
  (case ident
    "fn"     :fn
    "let"    :let
    "true"   :true
    "false"  :false
    "if"     :if
    "else"   :else
    "return" :return
             :ident))

(defn create [kind literal position]
  [kind literal position])

(def kind     first)
(def literal  second)
(def position third)
