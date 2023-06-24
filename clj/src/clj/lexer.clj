(ns clj.lexer
  (:refer-clojure :exclude [next peek])  
  (:require [jdsl.combinator  :as jc]
            [jdsl.basic       :as jb]
            [jdsl.char-stream :as cs]
            [jdsl.char-parser :as jp]
            [clj.token        :as token]))

(def single-char
  (-> token/create (jc/map (jp/any-of "=+-!*/<>,;(){}"))))

(def double-char
  (let [parsers (-> jp/string (map ["<=" ">=" "==" "!="]))] 
  (-> token/create (jc/map (jc/choice parsers)))))

(def string
  (let [parser (-> (jc/many* (jp/none-of "\""))
                   (jc/between (jp/char \") (jp/char \")))]
  (-> (partial token/create :string) (jc/map parser))))

(def integer
  (-> (partial token/create :int) (jc/map (jc/many+ jp/digit))))

(def keywords
  (let [parsers (-> jp/string (map ["fn" "let" "true" "false" "if" "else" "return"]))]
  (-> token/create (jc/map (jc/choice parsers)))))

(def identifier
  (-> (partial token/create :ident) (jc/map (jc/many+ jp/alpha-num))))

(def EOF
  (-> token/create (jc/map jp/eof)))

(def illegal
  (-> token/create (jc/map jp/any-char)))

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
    (if-not (= kind (token/kind token))
      (jc/fail-with (jb/error "Expected: " kind))
    (-> next))))

(defn run 
  ([input]
    (run lexer (cs/create input)))
  ([parser cs]
    (let [[token cs] (jb/run parser cs)]
    (if (= :eof (token/kind token))
      (list token)
    (cons token (lazy-seq (run parser cs)))))))

(comment
  (run "=+(){},;|")
  (run "== != <= >= < > = /")
  (run (slurp "./test/clj/lexer.monkey"))
  ())
