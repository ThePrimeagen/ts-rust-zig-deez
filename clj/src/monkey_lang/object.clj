(ns monkey-lang.object
  (:refer-clojure :exclude [boolean fn hash])
  (:require [monkey-lang.util :refer [third]]
            [monkey-lang.ast :as ast]
            [clojure.string :as str]))

(def ^:const INTEGER  :integer)
(def ^:const BOOLEAN  :boolean)
(def ^:const NULL     :null)
(def ^:const RETURN   :return)
(def ^:const ERROR    :error)
(def ^:const FUNCTION :fn)
(def ^:const STRING   :string)
(def ^:const BUILTIN  :builtin)
(def ^:const ARRAY    :array)
(def ^:const HASH     :hash)
(def ^:const HASH-PAIR :hash-pair)

(def ^:const True  [BOOLEAN true])
(def ^:const False [BOOLEAN false])
(def ^:const Null  [NULL    nil])

(def kind  first)
(def value second)

(defmacro integer [v]
  `(vector ~INTEGER ~v))

(defmacro string [v]
  `(vector ~STRING ~v))

(defmacro boolean [v]
  `(if ~v ~True ~False))

(defmacro array [elements]
  `(vector ~ARRAY ~elements))

(defmacro hash [pairs]
  `(vector ~HASH ~pairs))

(defmacro hash-pair [pair]
  `(vector ~HASH-PAIR ~pair))

(def hash-value (comp value value))

(defn hash-key [obj]
  (case (kind obj)
    :boolean (if (value obj) 1 0)
    :integer (value obj)
    :string  (hash (value obj))
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
    :string (str \" (value obj) \")
    :fn    (ast/to-str (fn-ast obj))
    :array (let [elements (mapv inspect (value obj))]
           (str "[" (str/join ", " elements) "]"))
    :hash-pair
           (->> (value obj)
                (mapv inspect)
                (str/join ": "))
    :hash  (let [pairs (for [pair (value obj)]
                         (inspect (second pair)))]
           (str "{" (str/join ", " pairs) "}"))
    (str (value obj))))
