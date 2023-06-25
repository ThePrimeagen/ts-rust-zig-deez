(ns monkey-lang.util)

(def third  #(nth % 2 nil))
(def fourth #(nth % 3 nil))

(defn to-str [chrs]
  (cond (seqable? chrs) (apply str chrs) 
        :else           (str chrs)))
