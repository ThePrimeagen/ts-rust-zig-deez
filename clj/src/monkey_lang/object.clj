(ns monkey-lang.object
  (:refer-clojure :exclude [boolean]))

(def ^:const INTEGER :integer)
(def ^:const BOOLEAN :boolean)
(def ^:const NULL    :null)
(def ^:const RETURN  :return)
(def ^:const ERROR   :error)

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

(def kind  first)
(def value second)
(def inspect (comp str value))
