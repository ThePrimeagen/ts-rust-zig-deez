(ns monkey-lang.lexer
  (:refer-clojure :exclude [next peek])  
  (:require [jdsl.combinator   :as jc]
            [jdsl.basic        :as jb]
            [jdsl.char-stream  :as cs]
            [jdsl.char-parser  :as jp]
            [jdsl.util         :as ju]
            [monkey-lang.token :as token]
            [monkey-lang.util  :as util]))

(defn single-char [cs]
  (when-let [[lit cs] (cs/read cs)]
  (when-let [kind (token/char1->kind lit)]
    [(token/create kind lit) cs])))

(defn double-char [cs]
  (when-let [[lit cs] (cs/read cs 2)]
  (when-let [kind (token/char2->kind lit)]
    [(token/create kind lit) cs])))

(def string-parser
  (-> (jc/many* (jp/none-of "\""))
      (jc/between (jp/char \") (jp/char \"))))

(def string
  (jb/do
    (lit <- string-parser)
    (jc/return (token/create token/STRING lit))))

(def integer
  (jb/do
    (lit <- (jc/many+ jp/digit))
    (jc/return (token/create token/INT lit))))

(def identifier
  (jp/satisfy (some-fn ju/alpha-num? #{\! \_})))

(def keywords
  (jb/do
    (chars <- (jc/many+ identifier))
    (let [lit (util/to-str chars)]
    (if-let [kind (token/ident->kind lit)]
      (jc/return (token/create kind lit))
    (jc/return (token/create token/IDENT lit))))))

(def EOF
  (jb/do
    (lit <- jp/eof)
    (jc/return (token/create token/EOF lit))))

(def illegal
  (jb/do
    (lit <- (jc/map jp/any-char))
    (jc/return (token/create token/ILLEGAL lit))))

(def tokens
  (jc/choice [double-char 
              single-char 
              string 
              integer 
              (jc/attempt keywords)
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
  (run "let x1 = 5")
  ())
