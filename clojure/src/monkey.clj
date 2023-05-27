(ns monkey
  (:import [java.lang Character]))

(def keywords
  {"fn" :function
   "let" :let})

(defn monkey-keyword?
  [word]
  (contains? keywords word))

(defn letter?
  [^Character ch]
  (or (Character/isLetter ch)
      (= \_ ch)))

(defn read-word-or-token
  [input]
  (let [[word rest] (split-with letter? input)
        word (apply str word)]
    [rest
     (if (monkey-keyword? word)
       {:token (get keywords word)}
       {:token :ident, :literal word})]))

(defn monkey-number?
  [^Character ch]
  (Character/isDigit ch))

(defn read-number
  [input]
  (let [[word rest] (split-with monkey-number? input)
        word (apply str word)]
    [rest
     {:token :int, :literal (Integer/parseInt word)}]))

(defn skip-whitespace
  [input]
  (drop-while #(Character/isWhitespace %) input))

(defn ch->token
  [ch]
  (case ch
    \{ {:token :lbrace}
    \} {:token :rbrace}
    \( {:token :lparen}
    \) {:token :rparen}
    \, {:token :comma}
    \; {:token :semicolon}
    \+ {:token :plus}
    \= {:token :equal}
    {:token :illegal, :literal ch}))

(defn parse
  ([input] (parse (skip-whitespace input) []))
  ([input tokens]
   (if (empty? input)
     (conj tokens {:token :eof})
     (let [[ch & rest] input
           [rest token] (cond
                          (letter? ch) (read-word-or-token input)
                          (monkey-number? ch) (read-number input)
                          :else [rest (ch->token ch)])]
       (recur (skip-whitespace rest)
              (conj tokens token))))))
