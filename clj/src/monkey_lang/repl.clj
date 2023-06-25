(ns monkey-lang.repl 
  (:require [monkey-lang.parser :as parser]
            [monkey-lang.ast :as ast]))

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
        (ast/pprint (parser/run input))
      (catch clojure.lang.ExceptionInfo e
        (parser/print-error e)))
      (recur))))))
