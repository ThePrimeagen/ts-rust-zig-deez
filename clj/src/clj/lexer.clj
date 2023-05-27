(ns clj.lexer
  (:use [clj.token]))

(set! *warn-on-reflection* true)

;; used ^Type to avoid reflection to boost performance
(defn space? [^Character chr]
  (Character/isWhitespace chr))

(defn letter? [^Character chr]
  (Character/isLetter chr))

(defn digit? [^Character chr]
  (Character/isDigit chr))

(defn to-str [chrs]
  (if (char? chrs) (str chrs) (apply str chrs)))

(defn lex
  ([input]
    (lex 0 input))
  ([pos [ch & rst :as input]]
    (cond (empty? input)  (list (token :eof nil pos))
          (space? ch)     (lex (inc pos) rst) ;; skips whitespaces
          (chr-token? ch) (cons (token (chr->token-type ch) ch pos)
                                (lazy-seq (lex (inc pos) rst)))
          (digit? ch)     (lex digit? pos input :int)
          (letter? ch)    (lex letter? pos input nil)
          :else           (list (token :illegal ch pos))))
  ([pred pos input type]
    "for lexing ints, keywords, idents based on predicate"
    (let [[chrs rst] (split-with pred input)
          ident      (to-str chrs)
          npos       (+ pos (count chrs))
          type       (or type (ident->token-type ident))]
    (cons (token type ident pos)
          (lazy-seq (lex npos rst))))))

(comment
  (lex "=+(){},;")
  (lex (slurp "./test/clj/input.monkey")))
