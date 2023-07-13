(ns monkey-lang.eval 
  (:require [monkey-lang.ast     :as ast]
            [monkey-lang.object  :as object]
            [monkey-lang.env     :as env]
            [monkey-lang.builtin :as builtin]))

(declare run eval-exprs)

(defn expand-tail-call [env ast rst]
  (case (ast/kind ast)
    :ast/expr-stmt    (let [expr (ast/expr-expr ast)]
                      (case (ast/kind expr)
                        :ast/call-expr (when (empty? rst)
                                         (recur env (ast/expr-expr ast) rst))
                        :ast/if-expr   (recur env (ast/expr-expr ast) rst)
                        #_else         (-> nil)))
    :ast/return-stmt  (recur env (ast/return-expr ast) rst)
    :ast/if-expr      (let [condi (run env (ast/if-condition ast))]
                      (if (object/error? condi)
                        {:error condi}
                      (if (object/value condi)
                        (if (empty? (ast/block-stmts (ast/if-consequence ast)))
                          {:result object/Null}
                        [env (ast/block-stmts (ast/if-consequence ast))])
                      (if (empty? (ast/block-stmts (ast/if-alternative ast)))
                        {:result object/Null}
                      [env (ast/block-stmts (ast/if-alternative ast))]))))
    
    :ast/call-expr  (let [func (run env (ast/call-fn ast))]
                    (if (object/error? func)
                      {:error func}
                    (let [args (eval-exprs env (ast/call-args ast))]
                    (if (object/error? args)
                      {:error args}
                    (if (object/is? func object/BUILTIN)
                      (let [result (builtin/invoke (object/value func) args)]
                      {:result result})
                    (let [params (->> (ast/fn-params (object/fn-ast func)) 
                                      (mapv ast/ident-literal))
                          stmts  (ast/block-stmts (ast/fn-block (object/fn-ast func)))
                          nenv   (env/enclosed (object/fn-env func) (zipmap params args))]
                    [nenv stmts]))))))
    #_else          (-> nil)))

(defn eval-stmts [env stmts]
  (loop [env    env
         result object/Null
         stmts  stmts]
    (if (empty? stmts)
      (-> result)
    (let [[stmt & rst] stmts]
    (if-let [expanded (expand-tail-call env stmt rst)]
      (if-let [error (:error expanded)]
        (-> error)
      (if-let [result (:result expanded)]
        (recur env result rst)
      (let [[nenv nstmts] expanded]  
      (recur nenv result (concat nstmts rst)))))
    (let [result (run env stmt)]
    (if (or (object/is? result object/RETURN)
            (object/is? result object/ERROR))
      (-> result)
    (recur env result rst))))))))

(defn eval-prefix [operator right]
  (case operator
    "!" (object/boolean (not (object/value right)))
    "-" (if-not (object/is? right object/INTEGER)
          (object/error "Unknown Operator: %s %s" operator (object/kind right))
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
    ("-" "+" "/" "*")
      (object/integer ((op->fn operator) (object/value left) (object/value right)))
    (">" "<" ">=" "<=" "==" "!=")
      (object/boolean ((op->fn operator) (object/value left) (object/value right)))
    (object/error "Unknown Operator: %s %s %s" (object/kind left) operator (object/kind right))))

(defn eval-string-infix-expr [left operator right]
  (case operator
    ("+") (object/string (str (object/value left) (object/value right)))
    (object/error "Unknown Operator: %s %s %s" (object/kind left) operator (object/kind right))))

(defn eval-infix [left operator right]
  (if-not (= (object/kind left) (object/kind right))
    (object/error "Type Mismatch: %s %s %s" (object/kind left) operator (object/kind right))
  (if (and (object/is? left  object/INTEGER)
           (object/is? right object/INTEGER))
    (eval-integer-infix-expr left operator right)
  (if (and (object/is? left  object/STRING)
           (object/is? right object/STRING))
    (eval-string-infix-expr left operator right)
  (case operator
    ("=="
     "!=") (object/boolean ((op->fn operator) (object/value left) (object/value right)))
  (object/error "Unknown Operator: %s %s %s" (object/kind left) operator (object/kind right)))))))

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

(defn eval-index-expr [left index]
  (if (and (object/is? left object/ARRAY)
           (object/is? index object/INTEGER))
    (builtin/invoke builtin/index [left index])
  (if (object/is? left object/HASH)
    (builtin/invoke builtin/get- [left index])
  (object/error "Index operator not supported: %s" (object/kind left)))))

(defn eval-hash [env pairs]
  (loop [pairs pairs
         hashs (transient {})]
    (if (empty? pairs)
      (object/hash- (persistent! hashs))
    (let [[[k v] & rst] pairs
          kee           (run env k)]
    (if (object/error? kee)
      (-> kee)
    (let [hash-kee (object/hash-key kee)]
    (if-not hash-kee
      (object/error "Unusable as hash key: %s" (object/kind kee))
    (let [value (run env v)]
    (if (object/error? value)
      (-> value)
    (recur rst (assoc! hashs hash-kee (object/hash-pair [kee value]))))))))))))

(defn run
  ([ast] 
    (run (env/create) ast))
  ([env ast]
    (case (ast/kind ast)
      :ast/program  (let [value (eval-stmts env (ast/program-stmts ast))]
                    (if (object/is? value object/RETURN)
                      (object/value value)
                    (-> value)))
      ;; statements
      :ast/let-stmt (let [value (run env (ast/let-value ast))]
                    (if (object/error? value)
                      (-> value)
                    (env/set! env (ast/let-ident ast) value)))
      
      :ast/expr-stmt    (recur env (ast/expr-expr ast))
      :ast/block-stmt   (eval-stmts env (ast/block-stmts ast))
      :ast/return-stmt  (let [value (run env (ast/return-expr ast))]
                        (if (object/error? value)
                          (-> value)
                        (object/return value)))
      ;; expressions
      :ast/prefix-expr  (let [right (run env (ast/prefix-right ast))]
                        (if (object/error? right)
                          (-> right)
                        (eval-prefix (ast/prefix-op ast) right)))
      
      :ast/infix-expr   (let [left (run env (ast/infix-left ast))]
                        (if (object/error? left)
                          (-> left)
                        (let [right (run env (ast/infix-right ast))]
                        (if (object/error? right)
                          (-> right)
                        (eval-infix left (ast/infix-op ast) right)))))
      
      :ast/if-expr      (let [condi (run env (ast/if-condition ast))]
                        (if (object/error? condi)
                          (-> condi)
                        (if (object/value condi)
                          (recur env (ast/if-consequence ast))
                        (if (empty? (ast/block-stmts (ast/if-alternative ast)))
                          (-> object/Null)
                        (recur env (ast/if-alternative ast))))))
      
      :ast/fn-lit     (object/fn ast env)
      :ast/call-expr  (let [func (run env (ast/call-fn ast))]
                      (if (object/error? func)
                        (-> func)
                      (let [args (eval-exprs env (ast/call-args ast))]
                      (if (object/error? args)
                        (-> args)
                      (if (object/is? func object/BUILTIN)
                        (builtin/invoke (object/value func) args)
                      (let [params (->> (ast/fn-params (object/fn-ast func)) 
                                        (mapv ast/ident-literal))
                            body   (ast/program (ast/block-stmts (ast/fn-block (object/fn-ast func))))
                            nenv   (env/enclosed (object/fn-env func) (zipmap params args))]
                      (recur nenv body)))))))
      
      :ast/index-expr (let [left (run env (ast/index-expr-left ast))]
                      (if (object/error? left)
                        (-> left)
                      (let [index (run env (ast/index-expr-index ast))]
                      (if (object/error? index)
                        (-> index)
                      (eval-index-expr left index)))))
      ;; literals
      :ast/ident-lit  (if-let [value (env/get env (ast/ident-literal ast))]
                        (-> value)
                      (if-let [builtin (get builtin/fn (ast/ident-literal ast) nil)]
                        (-> builtin)
                      (object/error "Identifier not found: %s" (ast/ident-literal ast))))
      
      :ast/int-lit    (object/integer (ast/int-value ast))
      :ast/bool-lit   (object/boolean (ast/bool-value ast))
      :ast/string-lit (object/string (ast/string-value ast))
      :ast/array-lit  (let [elements (eval-exprs env (ast/array-elements ast))]
                      (if (object/error? elements)
                        (-> elements)
                      (object/array (vec elements))))
      
      :ast/hash-lit   (eval-hash env (ast/hash-pairs ast))
      (assert ast (str "eval/run not implemented for " ast)))))

(comment
  "let fib = fn(n, a, b) { if (n == 0) { return a; } if (n == 1) { return b; } return fib(n - 1, b, n); };"
  )