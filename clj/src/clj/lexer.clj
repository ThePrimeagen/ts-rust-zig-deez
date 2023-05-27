(ns clj.lexer
  (:use [clj.util]
        [clj.token :as token :only [chr-token?]]))

(defn lex
  ([input]
    (lex 0 input))
  ([pos [ch & rst :as input]]
    (cond (empty? input)  (list (token/create :eof nil pos))
          (space? ch)     (lex (inc pos) rst) ;; skips whitespaces
          (chr-token? ch) (cons (token/create (token/chr->kind ch) ch pos)
                                (lazy-seq (lex (inc pos) rst)))
          (digit? ch)     (lex digit? pos input :int)
          (letter? ch)    (lex letter? pos input nil)
          :else           (list (token/create :illegal ch pos))))
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
  (lex (slurp "./test/clj/input.monkey")))
