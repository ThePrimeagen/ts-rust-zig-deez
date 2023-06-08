(#%require (only racket/base hash-iterate-first hash-iterate-next hash-iterate-key hash-iterate-value exit))
(load "../src/utils.rkt")
(loader "ast")
(loader "object")
(loader "token")

(define BUILDIN_FUNCTIONS (make-hash))

(hash-set! BUILDIN_FUNCTIONS "len" (new-buildin (lambda (args)
                                                  (if (or (not (pair? args)) (not (= (length args) 1))) (format-error "wrong number of arguments. got=" (number->string (length args)) ", want=1")
                                                      (cond
                                                        ((obj-string? (car args)) (new-int (string-length (string-value (car args)))))
                                                        ((obj-array? (car args)) (new-int (length (obj-value (car args)))))
                                                        (else (format-error "argument to 'len' not supported, got " (obj-type (car args)))))))))

(hash-set! BUILDIN_FUNCTIONS "first" (new-buildin (lambda (args)
                                                    (if (or (not (pair? args)) (not (= (length args) 1))) (format-error "wrong number of arguments. got=" (number->string (length args)) ", want=1")
                                                        (if (obj-array? (car args)) (let* ((el (obj-value (car args)))) (if (> (length el) 0) (car el) THE_NULL)) (format-error "Argument to `first` must be ARRAY got:" (obj-type (car args))))))))

(hash-set! BUILDIN_FUNCTIONS "rest" (new-buildin (lambda (args)
                                                    (if (or (not (pair? args)) (not (= (length args) 1))) (format-error "wrong number of arguments. got=" (number->string (length args)) ", want=1")
                                                        (if (obj-array? (car args)) (let* ((el (obj-value (car args)))) (if (> (length el) 0) (new-array-obj (cdr el)) THE_NULL)) (format-error "Argument to `rest` must be ARRAY got:" (obj-type (car args))))))))

; EW IMAGE PROVIDING A BUILDIN LAST FUNCTION. CADR THEM LISTS
(hash-set! BUILDIN_FUNCTIONS "last" (new-buildin (lambda (args)
                                                    (if (or (not (pair? args)) (not (= (length args) 1))) (format-error "wrong number of arguments. got=" (number->string (length args)) ", want=1")
                                                        (if (obj-array? (car args)) (let* ((el (obj-value (car args)))) (if (> (length el) 0) (get-nth-element el (- (length el) 1)) THE_NULL)) (format-error "Argument to `rest` must be ARRAY got:" (obj-type (car args))))))))
; DISGUSTING.
(hash-set! BUILDIN_FUNCTIONS "push" (new-buildin (lambda (args)
                                                    (if (or (not (pair? args)) (not (= (length args) 2))) (format-error "wrong number of arguments. got=" (number->string (length args)) ", want=2")
                                                        (if (obj-array? (car args)) (let* ((el (obj-value (car args)))) (new-array-obj (add-to-list el (cadr args)))) (format-error "First argument to `push` must be ARRAY got:" (obj-type (car args))))))))

(hash-set! BUILDIN_FUNCTIONS "puts" (new-buildin (lambda (args) (for-each (lambda (arg) (display (inspect arg)) (newline)) args) THE_NULL)))

(hash-set! BUILDIN_FUNCTIONS "exit" (new-buildin (lambda (args) (exit))))




(define (is-error? obj)
  (if (obj-null? obj) #f (obj-error? obj)))

(define (eval-program stmts env)
  (if (null? stmts) THE_NULL
  (let* ((stmt (car stmts)) (result (monkey-eval stmt env)))
    (cond
      ((obj-return-value? result) (obj-value result))
      ((obj-error? result) result)
      ((> (length (cdr stmts)) 0) (eval-program (cdr stmts) env))
      (else result)))))

(define (eval-block-statement block env)
  (define (inner stmts)
    (let* ((stmt (car stmts)) (result (monkey-eval stmt env)))
      (cond
        ((and (not (obj-null? result)) (or (obj-return-value? result) (obj-error? result))) result)
        ((> (length (cdr stmts)) 0) (inner (cdr stmts)))
        (else result))))
  (inner (block-stmts block)))

(define (eval-bang-operator-expression right)
  (cond
    ((eq? right THE_TRUE) THE_FALSE)
    ((eq? right THE_FALSE) THE_TRUE)
    ((obj-null? right) THE_TRUE)
    (else THE_FALSE)))

(define (is-truthy obj)
  (cond
    ((obj-null? obj) #f)
    ((eq? obj THE_TRUE) #t)
    ((eq? obj THE_FALSE) #f)
    (else #t)))

(define (eval-minus-prefix-operator-expression right)
  (if (obj-int? right) (new-int (- (obj-value right))) (format-error "unknown operator: " "-" (obj-type right))))

(define (eval-prefix-expression operator right)
  (cond
    ((char-eq? operator "!") (eval-bang-operator-expression right))
    ((char-eq? operator "-") (eval-minus-prefix-operator-expression right))
    
    (else (format-error "unknown operator: " operator (obj-type right)))))

(define (eval-if-expression node env)
  (define condition (monkey-eval (if-cond node) env))
  (cond
    ((is-error? condition) condition)
    ((is-truthy condition) (monkey-eval (if-cons node) env))
    ((not (obj-null? (if-alt node))) (monkey-eval (if-alt node) env))
    (else THE_NULL)))

(define (eval-integer-infix-expression operator left right)
  (define left-value (obj-value left))
  (define right-value (obj-value right))
  (cond
    ((char-eq? operator "+") (new-int (+ left-value right-value)))
    ((char-eq? operator "-") (new-int (- left-value right-value)))
    ((char-eq? operator "/") (new-int (/ left-value right-value)))
    ((char-eq? operator "*") (new-int (* left-value right-value)))
    ((char-eq? operator "<") (new-bool-obj-from-native (< left-value right-value)))
    ((char-eq? operator ">") (new-bool-obj-from-native (> left-value right-value)))
    ((char-eq? operator "==") (new-bool-obj-from-native (= left-value right-value)))
    ((char-eq? operator "!=") (new-bool-obj-from-native (not (= left-value right-value))))

    (else (format-error "unknown operator: " (obj-type left) " " operator " " (obj-type right)))))

(define (eval-string-infix-expression operator left right)
  (cond
    ((char-eq? operator "+") (new-string-obj (string-append (obj-value left) (obj-value right))))
    (else (format-error "unknown operator: " (obj-type left) " " operator " " (obj-type right)))))

(define (eval-infix-expression operator left right)
  (cond
    ((and (obj-int? left) (obj-int? right)) (eval-integer-infix-expression operator left right))
    ((and (obj-string? left) (obj-string? right)) (eval-string-infix-expression operator left right))
    
    ((char-eq? operator "==") (new-bool-obj-from-native (eq? left right)))
    ((char-eq? operator "!=") (new-bool-obj-from-native (not (eq? left right))))

    ((not (eq? (obj-type left) (obj-type right))) (format-error "type mismatch: " (obj-type left) " " operator " " (obj-type right)))
    (else (format-error "unknown operator: " (obj-type left) " " operator " " (obj-type right)))))

(define (eval-index-expression left index)
  (cond
    ((and (obj-array? left) (obj-int? index)) (eval-array-index left index))
    ((obj-hash? left) (eval-hash-index-expression left index))

    (else (format-error "index operator not supported: " (obj-type left)))))

(define (eval-array-index array idx)
  (let* ((index (obj-value idx)) (array-el (obj-value array)))
  (if (or (< index 0) (>= index (length array-el))) THE_NULL (get-nth-element array-el index))))

(define (eval-hash-index-expression hash index)
  (if (not (can-hash? index))
      (format-error "unusable as hash key: " (obj-type index))
      (let* ((pair (hash-obj-ref hash (make-hash-key index))))
        (if (null? pair) THE_NULL (hash-pair-value pair)))))

(define (eval-identifier node env)
  (define buildin (hash-ref BUILDIN_FUNCTIONS (identifier-value node) '()))
  (if (null? buildin) (let* ((id (get-from-environment env (identifier-value node))))
  (if (null? id) (format-error "identifier not found: " (identifier-value node)) id)) buildin))

(define (eval-expressions exps env)
  (define (inner exps out)
    (define exp (car exps))
    (let* ((evaluated (monkey-eval exp env))) (if (is-error? evaluated) (list evaluated) (if (> (length (cdr exps)) 0) (inner (cdr exps) (add-to-list out evaluated)) (add-to-list out evaluated)))))
    (if (null? exps) '() (inner exps '())))

(define (apply-functions function args)
  (cond 
      ((obj-fn? function) (let* ((extended-env (extend-function-env function args)) (evaluated (monkey-eval (obj-fn-body function) extended-env))) (unwrap-return-value evaluated)))
      ((obj-buildin? function) ((buildin-function function) args))

      (else (format-error "not a function: " (obj-type function)))))

(define (extend-function-env function args)
  (define env (new-enclosed-environment (obj-fn-env function)))
  (define (inner params args)
    (define param (car params))
    (define arg (car args))
    (add-to-environment env (int-value param) arg)
    (if (> (length (cdr params)) 0) (inner (cdr params) (cdr args)) env))
  (inner (obj-fn-params function) args))

(define (unwrap-return-value obj)
  (if (obj-return-value? obj) (obj-value obj) obj))

(define (eval-hash-literal node env)
  (define node-hash (hash-obj-hash node))
  (define hash (make-hash))

  (define (inner pos)
    (if (not (eq? pos #f))
        (let* ((key (monkey-eval (hash-iterate-key node-hash pos) env)))
        (if (is-error? key) key
            (if (not (can-hash? key))
                (format-error "unusable as hash key: " (obj-type key))
                (let* ((value (monkey-eval (hash-iterate-value node-hash pos) env)))
                  (if (is-error? value) value
                      (begin (hash-set! hash (make-hash-key key) (new-hash-pair key value)) (inner (hash-iterate-next node-hash pos))))))))))
  (inner (hash-iterate-first node-hash))

  (new-hash-obj hash))

(define (monkey-eval node env)
  (cond
    ((is-parser? node) (eval-program (parser-stmts node) env))
    ((exp-node? node) (monkey-eval (exp-value node) env))
    ((int-literal? node) (new-int (int-value node)))
    ((bool-literal? node) (new-bool-obj-from-native (bool-value node)))
    ((prefix-node? node) (let* ((right (monkey-eval (prefix-right node) env))) (if (is-error? right) right (eval-prefix-expression (prefix-operator node) right))))
    ((infix-node? node) (let* ((left (monkey-eval (infix-left node) env))) (if (is-error? left) left (let* ((right (monkey-eval (infix-right node) env))) (eval-infix-expression (infix-operator node) left right)))))
    ((block-stmt? node) (eval-block-statement node env))
    ((if-node? node) (eval-if-expression node env))
    ((return-node? node) (let* ((val (monkey-eval (return-value node) env))) (if (is-error? val) val (new-return-value val))))
    ((let-node? node) (let* ((val (monkey-eval (let-value node) env))) (if (is-error? val) val (begin (add-to-environment env (let-name node) val) THE_NULL))))
    ((identifier-node? node) (eval-identifier node env))
    ((fn? node) (new-function (fn-params node) (fn-body node) env))
    ((call-exp? node) (let* ((function (monkey-eval (call-fn node) env)))
                        (if (is-error? function) function
                            (let* ((args (eval-expressions (call-args node) env)))
                              (if (and (= (length args) 1) (is-error? (car args))) (car args) (apply-functions function args))))))
    ((string-literal? node) (new-string-obj (string-value node)))
    ((array? node) (let* ((elements (eval-expressions (obj-value node) env))) (if (and (= (length elements) 1) (is-error? node)) (car elements) (new-array-obj elements))))
    ((index-node? node) (let* ((left (monkey-eval (index-left node) env))) (if (is-error? left) left (begin (let* ((index (monkey-eval (index-index node) env))) (if (is-error? index) index (eval-index-expression left index)))))))
    ((hash-literal? node) (eval-hash-literal node env))
    (else THE_NULL)))