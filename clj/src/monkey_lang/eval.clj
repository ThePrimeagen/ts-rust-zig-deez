(ns monkey-lang.eval 
  (:require [monkey-lang.ast    :as ast]
            [monkey-lang.object :as object]
            [monkey-lang.env    :as env]))

(declare run)

(defn eval-stmts [env stmts]
  (loop [stmts  stmts
         result (object/null)]
    (if (empty? stmts)
      (-> result)
    (let [[stmt & rst] stmts
          result       (run env stmt)]
    (if (or (= object/RETURN (object/kind result))
            (= object/ERROR (object/kind result)))
      (-> result)
    (recur rst result))))))

(defn eval-prefix [operator right]
  (case operator
    "!" (object/boolean (not (object/value right)))
    "-" (if-not (= object/INTEGER (object/kind right))
          (object/error (str "Unknown Operator: " operator (name (object/kind right))))
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

(defn eval-integer-infix-expr [left operator right]
  (case operator
    ("-"
     "+"
     "/"
     "*") (object/integer ((op->fn operator) (object/value left) (object/value right)))
    (">"
     "<"
     ">="
     "<="
     "=="
     "!=") (object/boolean ((op->fn operator) (object/value left) (object/value right)))
    (object/error (str "Unknown Operator: " (name (object/kind left)) \space operator \space (name (object/kind right))))))

(defn eval-string-infix-expr [left operator right]
  (case operator
    ("+") (object/string (str (object/value left) (object/value right)))
    (object/error (str "Unknown Operator: " (name (object/kind left)) \space operator \space (name (object/kind right))))))

(defn eval-infix [left operator right]
  (if-not (= (object/kind left) (object/kind right))
    (object/error (str "Type Mismatch: " (name (object/kind left)) \space operator \space (name (object/kind right))))
  (if (and (object/is? left  object/INTEGER)
           (object/is? right object/INTEGER))
    (eval-integer-infix-expr left operator right)
  (if (and (object/is? left  object/STRING)
           (object/is? right object/STRING))
    (eval-string-infix-expr left operator right)
  (case operator
    ("=="
     "!=") (object/boolean ((op->fn operator) (object/value left) (object/value right)))
  (object/error (str "Unknown Operator: " (name (object/kind left)) \space operator \space (name (object/kind right)))))))))

(defn eval-exprs [env exprs]
  (loop [exprs  exprs
         result []]
    (if (empty? exprs)
      (not-empty result)
    (let [[expr & rst] exprs
          value        (run env expr)]
    (if (object/error? value)
      (-> value)
    (recur rst (conj result value)))))))

(defn run
  ([ast] 
    (run (env/create) ast))
  ([env ast]
    (case (ast/kind ast)
      :program (let [value (eval-stmts env (ast/program-stmts ast))]
               (if (object/is? value object/RETURN)
                 (object/value value)
               (-> value)))
      ;; statements
      :let    (let [value (run env (ast/let-value ast))]
              (if (object/error? value)
                (-> value)
              (env/set! env (ast/let-ident ast) value)))
      :expr   (run env (ast/expr-expr ast))
      :block  (eval-stmts env (ast/block-stmts ast))
      :return (let [value (run env (ast/return-expr ast))]
              (if (object/error? value)
                (-> value)
              (object/return value)))
      ;; expressions
      :prefix (let [right (run env (ast/prefix-right ast))]
              (if (object/error? right)
                (-> right)
              (eval-prefix (ast/prefix-op ast) right)))
      :infix  (let [left (run env (ast/infix-left ast))]
              (if (object/error? left)
                (-> left)
              (let [right (run env (ast/infix-right ast))]
              (if (object/error? right)
                (-> right)
              (eval-infix left (ast/infix-op ast) right)))))
      :if     (let [condi (run env (ast/if-condition ast))]
              (if (object/error? condi)
                (-> condi)
              (if (object/value condi)
                (run env (ast/if-consequence ast))
              (if (empty? (ast/if-alternative ast))
                (object/null)
              (run env (ast/if-alternative ast))))))
      :fn     (object/fn ast env)
      :call   (let [func (run env (ast/call-fn ast))]
              (if (object/error? func)
                (-> func)
              (let [args (eval-exprs env (ast/call-args ast))]
              (if (object/error? args)
                (-> args)
              (let [params (->> (ast/fn-params (object/fn-ast func)) 
                                (mapv ast/ident-literal))
                    body   (ast/fn-block (object/fn-ast func))
                    nenv   (env/enclosed (object/fn-env func) (zipmap params args))
                    value  (run nenv body)]
              (if (object/is? value object/RETURN)
                (object/value value)
              (-> value)))))))
      ;; literals
      :ident (if-let [value (env/get env (ast/ident-literal ast))]
              (-> value)
            (object/error (str "Identifier not found: " (ast/ident-literal ast))))
      :int   (object/integer (ast/int-value ast))
      :bool  (object/boolean (ast/bool-value ast))
      :string (object/string (ast/string-value ast))
      (assert ast (str "eval/run not implemented for " ast)))))
