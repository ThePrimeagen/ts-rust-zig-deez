(ns clj.token
  (:use [clj.util :only [third]]
        [clojure.pprint :only [pprint]]))

(defn chr->kind [chr]
  (case chr 
    \=   :assign
    \+   :plus
    \-   :minus
    \!   :bang
    \*   :astrisk
    \/   :slash
    \<   :lt
    \>   :gt
    \,   :comma
    \;   :semicolon
    \(   :l_paren
    \)   :r_paren
    \{   :l_squirly
    \}   :r_squirly
    "<=" :lteq
    ">=" :gteq
    "==" :eq
    "!=" :noteq
         nil))

(def chr-token? chr->kind)

(def two-op? #{">=" "<=" "==" "!="})

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

(defn pp [token]
  (-> (zipmap [:kind :literal :position] token)
      (pprint)))
