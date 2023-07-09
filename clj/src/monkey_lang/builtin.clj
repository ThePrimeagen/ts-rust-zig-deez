(ns monkey-lang.builtin 
  (:refer-clojure :exclude [fn rest last])
  (:require [monkey-lang.object :as object]))

(defn invoke [f args]
  (try
    (if-let [value (apply f args)]
      (-> value)
    (-> object/Null))
  (catch IndexOutOfBoundsException _
    (-> object/Null))
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

(defn get- [hash kee]
  (let [hash-kee (object/hash-key kee)]
  (if-not hash-kee
    (object/error (str "Unusable as hash key: " (name (object/kind kee))))
  (let [hash-pair (get (object/value hash) hash-kee nil)]
  (if-not hash-pair
    (-> object/Null)
  (object/hash-value hash-pair))))))

(def fn
  {"len"   (object/builtin  len)
   "puts"  (object/builtin  println-)
   "first" (object/builtin  first-)
   "last"  (object/builtin  last)
   "rest"  (object/builtin  rest)
   "push"  (object/builtin  push)})
