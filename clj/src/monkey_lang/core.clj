(ns monkey-lang.core
  (:require [monkey-lang.repl :as repl]))

#_{:clj-kondo/ignore [:unused-binding]}
(defn -main [& args]
  (repl/run))
