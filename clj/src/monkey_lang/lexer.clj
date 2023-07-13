(ns monkey-lang.lexer
  (:refer-clojure :exclude [next peek])  
  (:require [jdsl.combinator   :as jc]
            [jdsl.basic        :as jb]
            [jdsl.char-stream  :as cs]
            [jdsl.char-parser  :as jp]
            [monkey-lang.token :as token]))

(def single-char
  (-> token/create (jc/map (jp/any-of "=+-!*/<>,:;(){}[]"))))

(def double-char
  (let [double-char-parsers (-> jp/string (map ["<=" ">=" "==" "!="]))] 
  (-> token/create (jc/map (jc/choice double-char-parsers)))))

(def string
  (let [string-parser (-> (jc/many* (jp/none-of "\""))
                   (jc/between (jp/char \") (jp/char \")))]
  (-> (partial token/create token/STRING) (jc/map string-parser))))

(def integer
  (-> (partial token/create token/INT) (jc/map (jc/many+ jp/digit))))

(def keywords
  (let [keyword-parsers (-> jp/string (map ["fn" "let" "true" "false" "if" "else" "return" "null"]))]
  (-> token/create (jc/map (jc/choice keyword-parsers)))))

(def identifier
  (-> (partial token/create token/IDENT) (jc/map (jc/many+ jp/alpha-num))))

(def EOF
  (-> token/create (jc/map jp/eof)))

(def illegal
  (-> (partial token/create token/ILLEGAL) (jc/map jp/any-char)))

(def tokens
  (jc/choice [double-char 
              single-char 
              string 
              integer 
              keywords 
              identifier
              EOF
              illegal]))

(def lexer
  (-> jp/skip-spaces* (jc/>> tokens)))

(def next lexer)

(def peek (jc/peek next))

(defn expect [kind]
  (jb/do
    (token <- peek)
    (if-not (token/is? token kind)
      (jc/fail-with (jb/error "Expected: " kind))
    (-> next))))

(defn run 
  ([input]
    (run lexer (cs/create input)))
  ([parser cs]
    (let [[token cs] (jb/run parser cs)]
    (if (token/is? token token/EOF)
      (list token)
    (cons token (lazy-seq (run parser cs)))))))

(comment
  (run "=+(){},;|")
  (run "== != <= >= < > = /")
  (run (slurp "./test/clj/lexer.monkey"))
  ())
