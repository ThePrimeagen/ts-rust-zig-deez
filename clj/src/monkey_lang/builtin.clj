(ns monkey-lang.builtin 
  (:refer-clojure :exclude [fn])
  (:require [monkey-lang.object :as object]))

(defn invoke [[_ f wrapper] args]
  (try
    (wrapper (apply f args))
  (catch Exception e
    (object/error (ex-message e)))))

(def fn
  {"len"     (object/builtin count object/integer)
   "printf"  (object/builtin printf object/null)
   "println" (object/builtin println object/null)})