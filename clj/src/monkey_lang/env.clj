(ns monkey-lang.env
  (:refer-clojure :exclude [get set!])
  (:require [monkey-lang.object :as object]))

(defmacro create 
  ([]
    `(atom {:store {} :outer nil}))
  ([with]
    `(atom {:store ~with :outer nil})))

(defmacro enclosed 
  ([env]
    `(atom {:store {} :outer ~env}))
  ([env with]
    `(atom {:store ~with :outer ~env})))

(defn get [env k]
  (loop [env env]
    (if-let [value (get-in @env [:store k] nil)]
      [value env]
    (when-let [outer (:outer @env)]
      (recur outer)))))

(defmacro set-var! [env k v]
  `(do
     (swap! ~env assoc-in [:store ~k] ~v)
     (-> ~v)))

(defn set-vars! [env kvs]
  (doseq [[k v] kvs]
     (set-var! env k v))
  (-> env))

(defn exports [env]
  (loop [pairs (:store @env)
         hash-tbl (transient {})]
    (if (empty? pairs)
      (object/hash (persistent! hash-tbl))
    (let [[[k v] & rst] pairs]
    (if (object/is? v object/MODULE) ;; filtering out imported modules from exported hash
      (recur rst hash-tbl)
    (let [kee      (object/string k)
          hash-kee (object/hash-key kee)]
    (if-not hash-kee
      (object/error "Unusable as hash key: %s" (object/kind kee))
    (recur rst (assoc! hash-tbl hash-kee (object/hash-pair [kee v]))))))))))
