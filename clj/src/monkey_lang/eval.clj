(ns monkey-lang.eval 
  (:require [monkey-lang.ast    :as ast]
            [monkey-lang.object :as object]))

(declare run)

(defn eval-stmts [stmts]
  (loop [stmts  stmts
         result object/null]
    (if (empty? stmts)
      (-> result)
    (let [[stmt & rst] stmts
          result       (run stmt)]
    (if (or (= object/RETURN (object/kind result))
            (= object/ERROR (object/kind result)))
      (-> result)
    (recur rst result))))))

(defn eval-prefix [operator right]
  (case operator
    "!" (object/boolean (not (object/value right)))
    "-" (if-not (= object/INTEGER (object/kind right))
          (object/error (str "Unknown Operator: " operator (object/value right)))
        (object/integer (- (object/value right))))
    (assert operator (str "eval/eval-prefix not implemented for " operator))))

(def op->fn
  {"-"  -
   "+"  +
   "/"  /
   "*"  *
   "<"  <
   ">"  >
   "==" =
   "!=" not=
   ">=" >=
   "<=" <=})

(defn eval-infix [left operator right]
  (if-not (= (object/kind left) (object/kind right))
    (object/error (str "Type Mismatch: " (object/value left) \space operator \space (object/value right)))
  (case operator
    ("-"
     "+"
     "/"
     "*") (if-not (and (= object/INTEGER (object/kind left))
                       (= object/INTEGER (object/kind right)))
            (object/error (str "Unknown Operator: " (object/value left) \space operator \space (object/value right)))
          (object/integer ((op->fn operator) (object/value left) (object/value right))))
    (">"
     "<"
     ">="
     "<=") (if-not (and (= object/INTEGER (object/kind left))
                        (= object/INTEGER (object/kind right)))
             (object/error (str "Unknown Operator: " (object/value left) \space operator \space (object/value right)))
           (object/boolean ((op->fn operator) (object/value left) (object/value right))))
    ("=="
     "!=") (object/boolean ((op->fn operator) (object/value left) (object/value right)))
    (object/error (str "Unknown Operator: " (object/value left) \space operator \space (object/value right))))))

(defn run [ast]
  (case (ast/kind ast)
    :program (eval-stmts (ast/program-stmts ast))
    ;; statements
    :let    0 
    :expr   (run (ast/expr-expr ast))
    :block  (eval-stmts (ast/block-stmts ast))
    :return (object/return (object/value (run (ast/return-expr ast))))
    ;; expressions
    :prefix (eval-prefix (ast/prefix-op ast) 
                         (run (ast/prefix-right ast)))
    :infix  (eval-infix (run (ast/infix-left ast))
                        (ast/infix-op ast)
                        (run (ast/infix-right ast)))
    :if     (let [condi (run (ast/if-condition ast))]
            (if (object/value condi)
              (run (ast/if-consequence ast))
            (if (empty? (ast/if-alternative ast))
              object/null
            (run (ast/if-alternative ast)))))

    :fn     0
    :call   0
    ;; literals
    :ident 0
    :int   (object/integer (ast/int-value ast))
    :bool  (object/boolean (ast/bool-value ast))
    (assert ast (str "eval/run not implemented for " ast))))
