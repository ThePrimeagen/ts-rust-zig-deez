(ns monkey)

(def keywords
  {"fn" :function
   "let" :let})

(defn monkey-keyword?
  [word]
  (contains? keywords word))

(defn letter?
  [ch]
  (or (and (>= (int ch) (int \a))
           (<= (int ch) (int \z)))
      (and (>= (int ch) (int \A))
           (<= (int ch) (int \Z)))
      (= ch \_)))

(defn read-word
  [input]
  (loop [[ch & rest] input
         pos 0
         word []]
    (if (letter? ch)
      (recur rest (inc pos) (conj word ch))
      [(apply str word) pos])))

(defn read-word-or-token
  [input]
  (let [[word pos] (read-word input)]
    [(if (monkey-keyword? word)
       {:token (get keywords word)}
       {:token :ident, :literal word})
     pos]))

(defn monkey-number?
  [ch]
  (and (>= (int ch) (int \0))
       (<= (int ch) (int \9))))

(defn read-number
  [input]
  (loop [[ch & rest] input
         pos 0
         num []]
    (if (monkey-number? ch)
      (recur rest (inc pos) (conj num ch))
      [{:token :int, :literal (Integer/parseInt (apply str num))} pos])))

(defn skip-whitespace
  [input]
  (drop-while #(contains? #{\return \newline \tab \space} %) input))

(defn make-token
  [type]
  [{:token type} 1])

(defn tokenize
  ([input] (tokenize (skip-whitespace input) []))
  ([input tokens]
   (if (empty? input)
     (conj tokens {:token :eof})
     (let [current (first input)
           [token n] (cond
                       (= \{ current) (make-token :lbrace)
                       (= \} current) (make-token :rbrace)
                       (= \( current) (make-token :lparen)
                       (= \) current) (make-token :rparen)
                       (= \, current) (make-token :comma)
                       (= \; current) (make-token :semicolon)
                       (= \+ current) (make-token :plus)
                       (= \= current) (make-token :equal)
                       (letter? current) (read-word-or-token input)
                       (monkey-number? current) (read-number input)
                       :else {:token :illegal, :literal current})]
       (recur (skip-whitespace (drop n input))
              (conj tokens token))))))
