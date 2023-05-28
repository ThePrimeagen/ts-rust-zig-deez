(ns clj.lexer
  (:use [clj.util]
        [clj.token :as token :only [chr-token? two-op?]]))

(defn lex
  ([input]
    (lex 0 input))
  ([pos [ch pk & rst :as input]]
    (let [op (str ch pk)]
    (cond (empty? input)  (list (token/create :eof nil pos))
          (space? ch)     (lex (inc pos) (next input)) ;; skips whitespaces
          (two-op? op)    (cons (token/create (token/chr->kind op) op pos)
                                (lazy-seq (lex (+ 2 pos) rst)))
          (chr-token? ch) (cons (token/create (token/chr->kind ch) ch pos)
                                (lazy-seq (lex (inc pos) (next input))))
          (digit? ch)     (lex digit? pos input :int)
          (letter? ch)    (lex letter? pos input nil)
          :else           (list (token/create :illegal ch pos)))))
  ([pred pos input kind]
    "for lexing ints, keywords, idents based on predicate"
    (let [[chrs rst] (split-with pred input)
          ident      (to-str chrs)
          npos       (+ pos (count chrs))
          kind       (or kind (token/ident->kind ident))]
    (cons (token/create kind ident pos)
          (lazy-seq (lex npos rst))))))

(comment
  (lex "=+(){},;")
  (lex "== != <= >= < > = /")
  (lex (slurp "./test/clj/input.monkey"))
  ())
