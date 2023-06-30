(ns monkey-lang.object
  (:refer-clojure :exclude [boolean fn])
  (:require [monkey-lang.util :refer [third]]
            [monkey-lang.ast :as ast]))

(def ^:const INTEGER  :integer)
(def ^:const BOOLEAN  :boolean)
(def ^:const NULL     :null)
(def ^:const RETURN   :return)
(def ^:const ERROR    :error)
(def ^:const FUNCTION :fn)

(defmacro integer [v]
  `(vector ~INTEGER ~v))

(defmacro boolean [v]
  `(vector ~BOOLEAN ~v))

(def null [NULL nil])

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

(defn is [obj kynd]
  (= kynd (kind obj)))

(defn error? [obj]
  (= ERROR (kind obj)))

(defn inspect [obj]
  (case (kind obj)
    :fn  (ast/to-str (fn-ast obj))
    (str (value obj))))
