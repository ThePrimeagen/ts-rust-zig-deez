(ns monkey-lang.core
  (:require [monkey-lang.repl :as repl]))

(defn -main [& args]
  (repl/run))
