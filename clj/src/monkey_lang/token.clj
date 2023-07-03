(ns monkey-lang.token
  (:require [monkey-lang.util :as util]))

(def ^:const ILLEGAL   :illegal)
(def ^:const EOF       :eof)
(def ^:const IDENT     :ident)
(def ^:const INT       :int)
(def ^:const STRING    :string)
(def ^:const ASSIGN    :assign)
(def ^:const PLUS      :plus)
(def ^:const MINUS     :minus)
(def ^:const BANG      :bang)
(def ^:const ASTERISK  :astrisk)
(def ^:const SLASH     :slash)
(def ^:const LT        :lt)
(def ^:const GT        :gt)
(def ^:const EQ        :eq)
(def ^:const LT_EQ     :lteq)
(def ^:const GT_EQ     :gteq)
(def ^:const NOT_EQ    :noteq)
(def ^:const COMMA     :comma)
(def ^:const SEMICOLON :semicolon)
(def ^:const COLON     :colon)
(def ^:const LPAREN    :l_paren)
(def ^:const RPAREN    :r_paren)
(def ^:const LBRACE    :l_squirly)
(def ^:const RBRACE    :r_squirly)
(def ^:const LBRACKET  :l_bracket)
(def ^:const RBRACKET  :r_bracket)
(def ^:const FUNCTION  :fn)
(def ^:const LET       :let)
(def ^:const TRUE      :true)
(def ^:const FALSE     :false)
(def ^:const IF        :if)
(def ^:const ELSE      :else)
(def ^:const RETURN    :return)

(def lit->kind
  {"="  ASSIGN
   "+"  PLUS
   "-"  MINUS
   "!"  BANG
   "*"  ASTERISK
   "/"  SLASH
   "<"  LT
   ">"  GT
   ","  COMMA
   ";"  SEMICOLON
   ":"  COLON
   "("  LPAREN
   ")"  RPAREN
   "{"  LBRACE
   "}"  RBRACE
   "["  LBRACKET
   "]"  RBRACKET   
   "<=" LT_EQ
   ">=" GT_EQ
   "==" EQ
   "!=" NOT_EQ
   ""   EOF
   ;; keywords
   "fn"     FUNCTION
   "let"    LET
   "true"   TRUE
   "false"  FALSE
   "if"     IF
   "else"   ELSE
   "return" RETURN})

(defn create 
  ([literal]
    (let [literal (util/to-str literal)]
    (vector (or (lit->kind literal) ILLEGAL) literal)))
  ([kind literal]
    (vector kind (util/to-str literal))))

(def kind    first)
(def literal second)

(defn is? [token kynd]
  (= kynd (kind token)))
