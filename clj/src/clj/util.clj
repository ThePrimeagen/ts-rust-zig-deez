(ns clj.util)

(set! *warn-on-reflection* true)

(defn third [coll]
  (nth coll 2 nil))

;; used ^Type to avoid reflection to boost performance
(defn space? [^Character chr]
  (Character/isWhitespace chr))

(defn letter? [^Character chr]
  (or (Character/isLetter chr)
      (= chr \_)))

(defn digit? [^Character chr]
  (Character/isDigit chr))

(defn to-str [chrs]
  (cond (char? chrs) (str chrs) 
        :else        (apply str chrs)))
