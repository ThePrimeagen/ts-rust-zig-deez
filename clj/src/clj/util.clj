(ns clj.util)

(set! *warn-on-reflection* true)

(def third (comp second next))

;; used ^Type to avoid reflection to boost performance
(defn space? [^Character chr]
  (Character/isWhitespace chr))

(defn letter? [^Character chr]
  (or (Character/isLetter chr)
      (= chr \_)))

(defn digit? [^Character chr]
  (Character/isDigit chr))

(defn to-str [chrs]
  (cond (seqable? chrs) (apply str chrs) 
        :else           (str chrs)))
