(ns clj.token
  (:require [clj.util :as util]))

(def lit->kind
  {"="  :assign
   "+"  :plus
   "-"  :minus
   "!"  :bang
   "*"  :astrisk
   "/"  :slash
   "<"  :lt
   ">"  :gt
   ","  :comma
   ";"  :semicolon
   "("  :l_paren
   ")"  :r_paren
   "{"  :l_squirly
   "}"  :r_squirly
   "<=" :lteq
   ">=" :gteq
   "==" :eq
   "!=" :noteq
   ""   :eof
   ;; keywords
   "fn"     :fn
   "let"    :let
   "true"   :true
   "false"  :false
   "if"     :if
   "else"   :else
   "return" :return})

(defn create 
  ([literal]
    (let [literal (util/to-str literal)]
    (vector (-> (lit->kind literal) (or :illegal)) (util/to-str literal))))
  ([kind literal]
    (vector kind (util/to-str literal))))

(def kind     first)
(def literal  second)
