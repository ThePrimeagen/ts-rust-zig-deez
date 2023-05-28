(ns monkey
  (:import [java.lang Character]))

(def kind first)
(def position second)
(def literal #(nth % 2 nil))

(defn letter?
  [^Character ch]
  (or (Character/isLetter ch)
      (= \_ ch)))

(defn digit?
  [^Character ch]
  (Character/isDigit ch))

(defn space?
  [^Character ch]
  (Character/isWhitespace ch))

(defn ch->token
  [ch]
  (case ch
    \{ :lbrace
    \} :rbrace
    \( :lparen
    \) :rparen
    \, :comma
    \; :semicolon
    \+ :plus
    \= :equal
    nil))

(def ch-token? ch->token)

(defn ident->kind
  [ident]
  (case ident
    "fn" :function
    "let" :let
    :ident))

(defn parse
  ([input] (parse input 0 []))
  ([[ch & rest :as input] pos tokens]
   (if (empty? input)
     (conj tokens [:eof pos])
     (cond (space? ch) (recur rest (inc pos) tokens)
           (ch-token? ch) (recur rest (inc pos) (conj tokens [(ch->token ch) pos]))
           (digit? ch) (parse input pos tokens digit? :int)
           (letter? ch) (parse input pos tokens letter? nil)
           :else (recur rest (inc pos) (conj tokens [:illegal pos ch])))))
  ([input pos tokens pred kind]
   (let [[word rest] (split-with pred input)
         word (apply str word)
         npos (+ pos (count word))
         kind (or kind (ident->kind word))]
     (parse rest npos (conj tokens (case kind
                                     :ident [kind pos word]
                                     :int [kind pos (Integer/parseInt word)]
                                     [kind pos]))))))
