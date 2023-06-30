(ns monkey-lang.repl 
  (:require [monkey-lang.parser :as parser]
            [monkey-lang.eval   :as eval]
            [monkey-lang.env    :as env]
            [monkey-lang.object :as object]
            [monkey-lang.util   :refer [yellow]]))

(def ^:const welcome " 
              __,__          
     .--.  .-'     '-.  .--. 
    / .. |/  .-. .-.  |/ .. |
   | | '|   /   Y   |  |' | |
   | |  |   | 0 | 0 /  /  / |
    | '- ,|.-'''''''-./, -' /
     ''-' /_   ^ ^   _| '-'' 
         | |._     _./ |     
         |  |  '~'  /  /     
          '._ '-=-' _.'      
             '-----'          
  Welcome to Monkey Lang REPL. 
 (Type 'exit' and enter to exit)")

(defn run []
  (println welcome)
  (let [env (env/create)]
  (loop []
    (print ">> ") (flush)
    (let [input (read-line)]
    (if (empty? input)
      (recur)
    (when (not= input "exit")
      (try
        (when-let [obj (eval/run env (parser/run input))]
          (println (yellow (object/inspect obj))))
      (catch clojure.lang.ExceptionInfo e
        (parser/print-error e)))
      (recur)))))))
