(ns monkey-lang.util)

(def third  #(nth % 2 nil))
(def fourth #(nth % 3 nil))

(defn to-str [chrs]
  (cond (seqable? chrs) (apply str chrs) 
        :else           (str chrs)))

(defn debug [v]
  (println v)
  (-> v))

(defn yellow [txt]
  (str "\033[;33m" txt "\033[0m"))
