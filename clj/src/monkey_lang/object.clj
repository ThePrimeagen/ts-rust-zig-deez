(ns monkey-lang.object
  (:refer-clojure :exclude [boolean fn])
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

(defmacro integer [v]
  `(vector ~INTEGER ~v))

(defmacro string [v]
  `(vector ~STRING ~v))

(defmacro boolean [v]
  `(vector ~BOOLEAN ~v))

(defmacro array [elements]
  `(vector ~ARRAY ~elements))

(def null (constantly [NULL nil]))

(defmacro return [v]
  `(vector ~RETURN ~v))

(defmacro error [v]
  `(vector ~ERROR ~v))

(def message second)

(defmacro fn [ast env]
  `(vector ~FUNCTION ~ast ~env))

(def fn-ast    second)
(def fn-env    third)

(def kind  first)
(def value second)

(defmacro builtin [fn]
  `(vector ~BUILTIN ~fn))

(defn is? [obj kynd]
  (= kynd (kind obj)))

(defn error? [obj]
  (= ERROR (kind obj)))

(defn inspect [obj]
  (case (kind obj)
    :fn    (ast/to-str (fn-ast obj))
    :array (let [elements (mapv inspect (value obj))]
           (str "[" (str/join ", " elements) "]"))
    :hash  (let [pairs (for [pair (value obj)]
                         (str/join ": " (mapv inspect pair)))]
           (str "{" (str/join ", " pairs) "}"))
    (str (value obj))))
