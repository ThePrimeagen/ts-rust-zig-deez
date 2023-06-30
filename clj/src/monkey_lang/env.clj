(ns monkey-lang.env
  (:refer-clojure :exclude [get set!]))

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
      (-> value)
    (when-let [outer (:outer @env)]
      (recur outer)))))

(defmacro set! [env k v]
  `(do
     (swap! ~env assoc-in [:store ~k] ~v)
     (-> ~v)))
