(ns monkey-lang.lexer
  (:refer-clojure :exclude [next peek])  
  (:require [jdsl.combinator   :as jc]
            [jdsl.basic        :as jb]
            [jdsl.char-stream  :as cs]
            [jdsl.char-parser  :as jp]
            [jdsl.util         :as ju]
            [monkey-lang.token :as token]
            [monkey-lang.util  :as util]))

(def single-char
  (jb/do
    (lit <- (jp/satisfy token/char1->kind))
    (let [kind (token/char1->kind lit)]
    (jc/return (token/create kind lit)))))

(defn double-char [cs]
  (when-let [[lit cs] (cs/read cs 2)]
  (when-let [kind (token/char2->kind lit)]
    [(token/create kind lit) cs])))

(def string
  (let [string-parser (-> (jc/many* (jp/none-of "\""))
                          (jc/between (jp/char \") (jp/char \")))]
  (-> (partial token/create token/STRING) (jc/map string-parser))))

(def integer
  (-> (partial token/create token/INT) (jc/map (jc/many+ jp/digit))))

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
  (-> (partial token/create token/EOF) (jc/map jp/eof)))

(def illegal
  (-> (partial token/create token/ILLEGAL) (jc/map jp/any-char)))

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
