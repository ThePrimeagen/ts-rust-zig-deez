(ns monkey-lang.builtin 
  (:refer-clojure :exclude [fn rest last first])
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

(defn len [obj]
  (case (object/kind obj)
    (:object/array
     :object/string) (object/integer (count (object/value obj)))
  (object/error "len not implemented for %s" (object/kind obj))))

(defn puts [& values]
  (->> values
       (mapv object/value)
       (apply println)))

(defn index [obj idx]
  (if (object/is? obj object/ARRAY)
    (nth (object/value obj) (object/value idx) object/Null)
  (object/error "index not implemented for %s" (object/kind obj))))

(defn first [obj]
  (if (object/is? obj object/ARRAY)
    (clojure.core/first (object/value obj))
  (object/error "first not implemented for %s" (object/kind obj))))

(defn last [obj]
  (if (object/is? obj object/ARRAY)
    (peek (object/value obj))
  (object/error "last not implemented for %s" (object/kind obj))))

(defn rest [obj]
  (if (object/is? obj object/ARRAY)
    (object/array (subvec (object/value obj) 1))
  (object/error "rest not implemented for %s" (object/kind obj))))

(defn push [obj value]
  (if (object/is? obj object/ARRAY)
    (object/array (persistent! (conj! (transient (object/value obj)) value)))
  (object/error "push not implemented for %s" (object/kind obj))))

(defn push! [obj value]
  (if (object/is? obj object/ARRAY)
    (do
      (swap! (object/ref obj) conj value)
      (-> object/Null))
  (object/error "push! not implemented for %s" (object/kind obj))))

(defn at [obj kee]
  (let [hash-kee (object/hash-key kee)]
  (if-not hash-kee
    (object/error (str "Unusable as hash key: " (name (object/kind kee))))
  (let [hash-tbl  (object/value obj)
        hash-pair (get hash-tbl hash-kee nil)]
  (if-not hash-pair
    (-> object/Null)
  (object/hash-value hash-pair))))))

(def fn
  {"len"   (object/builtin len)
   "puts"  (object/builtin puts)
   "first" (object/builtin first)
   "last"  (object/builtin last)
   "rest"  (object/builtin rest)
   "push"  (object/builtin push)
   "push!" (object/builtin push!)})
