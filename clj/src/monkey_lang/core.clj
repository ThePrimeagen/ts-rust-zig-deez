(ns monkey-lang.core
  (:require [monkey-lang.repl :as repl]))

#_{:clj-kondo/ignore [:unused-binding]}
(defn -main [& args]
  (let [path (first *command-line-args*)]
  (cond (nil? path) (repl/run)
        :else       (println path))))
