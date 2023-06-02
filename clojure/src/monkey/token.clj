(ns monkey.token)

(defrecord Token [type literal] :load-ns true)

(defn new-token [type ch]
  {:pre [keyword? string?]}
  (->Token type ch))

(def token
  {:illegal "ILLEGAL"
   :eof "EOF"
   :ident "IDENT"
   :int "INT"
   :assign "="
   :plus "+"
   :minus "-"
   :bang "!"
   :asterisk "*"
   :slash "/"
   :lt "<"
   :gt ">"
   :eq "=="
   :not-eq "!="
   :comma ","
   :semicolon ";"
   :lparen "("
   :rparen ")"
   :lbrace "{"
   :rbrace "}"
   :fn "FUNCTION"
   :let "LET"
   :true "TRUE"
   :false "FALSE"
   :if "IF"
   :else "ELSE"
   :return "RETURN"})
