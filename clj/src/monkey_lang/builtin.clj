(ns monkey-lang.builtin 
  (:refer-clojure :exclude [fn rest])
  (:require [monkey-lang.object :as object]))

(defn invoke [[_ peel f wrap] args]
  (let [args (peel args)]
  (try
    (if-let [value (apply f args)]
      (wrap value)
    (object/null))
  (catch IndexOutOfBoundsException _
    (object/error "Index Out of Bounds"))
  (catch Exception e
    (object/error (ex-message e))))))

(defn rest [v]
  (subvec v 1))

(defn peel-all [args]
  (mapv object/value args))

(defn peel-fst [[fst & rst]]
  (cons (object/value fst) rst))

(def fn
  {"len"     (object/builtin peel-all count   object/integer)
   "printf"  (object/builtin peel-all printf  object/null)
   "println" (object/builtin peel-all println object/null)
   "index"   (object/builtin peel-all nth     object/id)
   "first"   (object/builtin peel-all first   object/id)
   "last"    (object/builtin peel-all peek    object/id)
   "rest"    (object/builtin peel-all rest    object/array)
   "push"    (object/builtin peel-fst conj    object/array)})
