(ns monkey-lang.repl 
  (:require [monkey-lang.parser :as parser]
            [monkey-lang.eval :as eval]
            [monkey-lang.object :as object]))

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
  (loop []
    (print ">> ") (flush)
    (let [input (read-line)]
    (if (empty? input)
      (recur)
    (when (not= input "exit")
      (try
        (when-let [obj (eval/run (parser/run input))]
          (println (object/inspect obj) \newline))
      (catch clojure.lang.ExceptionInfo e
        (parser/print-error e)))
      (recur))))))
