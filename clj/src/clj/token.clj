(ns clj.token)

(defn chr->token-type [chr]
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

(def chr-token? chr->token-type)

(defn ident->token-type [ident]
  (case ident
    "fn"  :fn
    "let" :let
          :ident))

(defn token [type literal position]
  [type literal position])

(def token-type first)
(def literal    second)
(def position   #(nth % 2 nil)) ;; third element
