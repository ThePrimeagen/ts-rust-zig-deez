(ns monkey-lang.builtin 
  (:refer-clojure :exclude [fn rest last])
  (:require [monkey-lang.object :as object]))

(defn invoke [f args]
  (try
    (if-let [value (apply f args)]
      (-> value)
    (object/null))
  (catch IndexOutOfBoundsException _
    (object/null))
  (catch Exception e
    (object/error (ex-message e)))))

(defn len [array]
  (-> (object/value array)
      (count)
      (object/integer)))

(defn println- [& values]
  (->> values
       (mapv object/inspect)
       (apply println)))

(defn index [array idx]
  (-> (object/value array)
      (nth (object/value idx) nil)))

(defn first- [array]
  (first (object/value array)))

(defn last [array]
  (peek (object/value array)))

(defn rest [array]
  (-> (object/value array)
      (subvec 1)
      (object/array)))

(defn push [array value]
  (-> (object/value array)
      (conj value)
      (object/array)))

(def fn
  {"len"     (object/builtin  len)
   "println" (object/builtin  println-)
   "index"   (object/builtin  index)
   "first"   (object/builtin  first-)
   "last"    (object/builtin  last)
   "rest"    (object/builtin  rest)
   "push"    (object/builtin  push)})
