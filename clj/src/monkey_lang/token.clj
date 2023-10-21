(ns monkey-lang.token
  (:require [monkey-lang.util :as util]))

(def ^:const ILLEGAL   :token/illegal)
(def ^:const EOF       :token/eof)
(def ^:const IDENT     :token/ident)
(def ^:const INT       :token/int)
(def ^:const FLOAT     :token/float)
(def ^:const HEX       :token/hex)
(def ^:const OCTAL     :token/octal)
(def ^:const BINARY    :token/binary)
(def ^:const STRING    :token/string)
(def ^:const ASSIGN    :token/assign)
(def ^:const PLUS      :token/plus)
(def ^:const MINUS     :token/minus)
(def ^:const BANG      :token/bang)
(def ^:const ASTERISK  :token/astrisk)
(def ^:const SLASH     :token/slash)
(def ^:const LT        :token/lt)
(def ^:const GT        :token/gt)
(def ^:const EQ        :token/eq)
(def ^:const LT_EQ     :token/lteq)
(def ^:const GT_EQ     :token/gteq)
(def ^:const NOT_EQ    :token/noteq)
(def ^:const COMMA     :token/comma)
(def ^:const SEMICOLON :token/semicolon)
(def ^:const COLON     :token/colon)
(def ^:const LPAREN    :token/l-paren)
(def ^:const RPAREN    :token/r-paren)
(def ^:const LBRACE    :token/l-brace)
(def ^:const RBRACE    :token/r-brace)
(def ^:const LBRACKET  :token/l-bracket)
(def ^:const RBRACKET  :token/r-bracket)
(def ^:const DOT       :token/dot)
(def ^:const FUNCTION  :token/fn)
(def ^:const LET       :token/let)
(def ^:const TRUE      :token/true)
(def ^:const FALSE     :token/false)
(def ^:const IF        :token/if)
(def ^:const ELSE      :token/else)
(def ^:const RETURN    :token/return)
(def ^:const FOR       :token/for)
(def ^:const WHILE     :token/while)
(def ^:const BREAK     :token/break)
(def ^:const CONTINUE  :token/continue)
(def ^:const IMPORT    :token/import)
(def ^:const NULL      :token/null)

(def char1->kind
  {\=  ASSIGN
   \+  PLUS
   \-  MINUS
   \!  BANG
   \*  ASTERISK
   \/  SLASH
   \<  LT
   \>  GT
   \,  COMMA
   \;  SEMICOLON
   \:  COLON
   \(  LPAREN
   \)  RPAREN
   \{  LBRACE
   \}  RBRACE
   \[  LBRACKET
   \]  RBRACKET
   \.  DOT})

(def char2->kind
  {[\< \=] LT_EQ
   [\> \=] GT_EQ
   [\= \=] EQ
   [\! \=] NOT_EQ})

(def ident->kind
  {"fn"     FUNCTION
   "let"    LET
   "true"   TRUE
   "false"  FALSE
   "if"     IF
   "else"   ELSE
   "return" RETURN
   "for"    FOR
   "while"  WHILE
   "break"  BREAK
   "continue" CONTINUE
   "import" IMPORT
   "null"   NULL})

(defmacro create [kind literal]
  `(vector ~kind (util/to-str ~literal)))

(def kind    first)
(def literal second)

(defmacro is? [token kynd]
  `(= ~kynd (kind ~token)))
