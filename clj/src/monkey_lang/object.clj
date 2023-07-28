(ns monkey-lang.object
  (:refer-clojure :exclude [boolean fn hash ref])
  (:require [monkey-lang.util :refer [third]]
            [monkey-lang.ast :as ast]
            [clojure.string :as str]))

(def ^:const INTEGER   :object/integer)
(def ^:const BOOLEAN   :object/boolean)
(def ^:const NULL      :object/null)
(def ^:const RETURN    :object/return)
(def ^:const ERROR     :object/error)
(def ^:const FUNCTION  :object/fn)
(def ^:const STRING    :object/string)
(def ^:const BUILTIN   :object/builtin)
(def ^:const ARRAY     :object/array)
(def ^:const HASH      :object/hash)
(def ^:const HASH-PAIR :object/hash-pair)

(def ^:const True  [BOOLEAN true])
(def ^:const False [BOOLEAN false])
(def ^:const Null  [NULL    nil])

(def kind  first)

(def ref second)

(defn value [obj]
  (case (kind obj)
    (:object/array
     :object/hash) (deref (second obj))
    #_else         (second obj)))

(defmacro integer [v]
  `(vector ~INTEGER ~v))

(defmacro string [v]
  `(vector ~STRING ~v))

(defmacro boolean [v]
  `(if ~v ~True ~False))

(defmacro array [elements]
  `(vector ~ARRAY (atom ~elements)))

(defmacro hash [pairs]
  `(vector ~HASH (atom ~pairs)))

(defmacro hash-pair [pair]
  `(vector ~HASH-PAIR ~pair))

(def hash-value (comp value value))

(defn hash-key [obj]
  (case (kind obj)
    :object/boolean (if (value obj) 1 0)
    :object/integer (value obj)
    :object/string  (clojure.core/hash (value obj))
                    nil))

(defmacro return [v]
  `(vector ~RETURN ~v))

(defmacro error 
  ([v]
    `(vector ~ERROR ~v))
  ([fmt & args]
    `(error (apply format ~fmt (mapv name [~@args])))))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def message second)

(defmacro fn [ast env]
  `(vector ~FUNCTION ~ast ~env))

(def fn-ast    second)
(def fn-env    third)

(defmacro builtin [fn]
  `(vector ~BUILTIN ~fn))

(defmacro is? [obj kynd]
  `(= ~kynd (kind ~obj)))

(defn error? [obj]
  (= ERROR (kind obj)))

(defn inspect [obj]
  (case (kind obj)
    :object/string (str \" (value obj) \")
    :object/fn     (ast/to-str (fn-ast obj))
    :object/array 
      (let [elements (mapv inspect (value obj))]
      (str "[" (str/join ", " elements) "]"))
    :object/hash-pair
      (->> (value obj)
           (mapv inspect)
           (str/join ": "))
    :object/hash
      (let [pairs (for [pair (value obj)]
                    (inspect (second pair)))]
                  (str "{" (str/join ", " pairs) "}"))
    :object/null  "null"
    (str (value obj))))
