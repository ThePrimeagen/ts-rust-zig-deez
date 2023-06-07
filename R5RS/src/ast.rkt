(load "../src/utils.rkt")
(loader "token")


; NODE
(define (node-type node)
  (car node))

(define (node-token node)
  (cadr node))


; EXPRESSION NODE
; (token value)
(define (new-exp-node token value)
  (list 'expression-node token value))

(define (exp-node? node)
  (tagged-list? node 'expression-node))

(define (exp-value s)
  (caddr s))


; IDENTENTIFIER STATEMENT
; (token value)
(define (new-identifier-node token value)
  (list 'identifier-node token value))

(define (new-identifier-node-from-token token)
  (new-identifier-node token (token-literal token)))

(define (new-identifier-node-from-type type)
  (new-identifier-node-from-token (token-from-type type)))

(define (identifier-node? s)
  (tagged-list? s 'identifier-node))

(define (identifier-value id)
  (caddr id))


; LET NODE
; (token exp)
(define (new-let-node name value)
  (list 'let-node (to-token LET "let") name value))

(define (let-node? node)
  (tagged-list? node 'let-node))

(define (set-let-value! node value)
  (if (not (null? node)) (set-car! (cdddr node) value)))

(define (let-value node)
  (cadddr node))

(define (let-id node)
  (caddr node))

(define (let-name node)
  (identifier-value (let-id node)))


; RETURN NODE
; (token value)
(define (new-return-node value)
  (list 'return-node (token-from-type RETURN) value))

(define (return-node? node)
  (tagged-list? node 'return-node))

(define (return-value node)
  (caddr node))


; PREFIX NODE
; (token operator right)
(define (new-prefix-node token operator right)
  (list 'prefix-node token operator right))

(define (prefix-node? node)
  (tagged-list? node 'prefix-node))

(define (prefix-right node)
  (car (cdddr node)))

(define (prefix-operator node)
  (caddr node))


; INFIX NODE
; (token left operator right)
(define (new-infix-node token left operator right)
  (list 'infix-node token left operator right))

(define (infix-node? node)
  (tagged-list? node 'infix-node))

(define (infix-left node)
  (caddr node))

(define (infix-operator node)
  (car (cdddr node)))

(define (infix-right node)
  (cadr (cdddr node)))


; INDEX NODE
; (token left index)
(define (new-index-node token left index)
  (list 'index-node token left index))

(define (index-node? node)
  (tagged-list? node 'index-node))

(define (index-left node)
  (caddr node))

(define (index-index node)
  (car (cdddr node)))


; INTEGER LITERAL
; (token value)
(define (new-integeral-literal token value)
  (list 'integeral-literal token value))

(define (int-literal? s)
  (tagged-list? s 'integeral-literal))

(define (int-value literal)
  (caddr literal))


; BOOLEAN
; (token value)
(define (new-bool token value)
  (list 'bool token value))

(define (bool-literal? s)
  (tagged-list? s 'bool))

(define (bool-value literal)
  (caddr literal))


; STRING LITERAL
; (token value)
(define (new-string token value)
  (list 'string token value))

(define (string-literal? s)
  (tagged-list? s 'string))

(define (string-value literal)
  (caddr literal))


; ARRAY LITERAL
; (token (expression))
(define (new-array token exps)
  (list 'array token exps))

(define (array? s)
  (tagged-list? s 'array))

(define (array-exps array)
  (caddr array))

; HASH LITERAL
; (token hash)
(define (new-hash-literal token)
  (list 'hash token (make-hash)))

(define (hash-literal? s)
  (tagged-list? s 'hash))

(define (hash-literal-hash hash)
  (caddr hash))

(define (hash-literal-set! h key value)
  (hash-set! (hash-literal-hash h) key value))

(define (hash-literal-ref h key)
  (hash-ref (hash-literal-hash h) key '()))


; IF NODE
; (token condition consequence alternative)
(define (new-if-node token condition consequence alternative)
  (list 'if-node token condition consequence alternative))

(define (if-node? node)
  (tagged-list? node 'if-node))

(define (if-cond node)
  (caddr node))

(define (if-cons node)
  (cadddr node))

(define (if-alt node)
  (cadr (cdddr node)))


; BLOCK STATEMENT
; (token (staments))
(define (new-block-stmt token stmts)
  (list 'block-stmt token stmts))

(define (block-stmt? s)
  (tagged-list? s 'block-stmt))

(define (block-token s)
  (cadr s))

(define (block-stmts s)
  (caddr s))

(define (add-stmt-to-block block stmt)
  (set-car! (cddr block) (add-to-list (block-stmts block) stmt)))


; FUNCTION LITRERAL
; (token (id) block)
(define (new-fn token params body)
  (list 'fn-literal token params body))

(define (fn? s)
  (tagged-list? s 'fn-literal))

(define (fn-token s)
  (cadr s))

(define (fn-params s)
  (caddr s))

(define (fn-body s)
  (cadddr s))

(define (add-param-to-fn! fn param)
  (set-car! (cddr s) (add-to-list (fn-params fn) param)))


; CALL EXPRESSION
; (token function (arguments))
(define (new-call-expression token function arguments)
  (list 'call-expression token function arguments))

(define (call-exp? s)
  (tagged-list? s 'call-expression))

(define (call-args c)
  (car (cdddr c)))

(define (call-fn c)
  (caddr c))

(define (call-token c)
  (cadr c))

; SHARED METHODS
(define (token-literal-from-state state)
  (token-literal (cadr state)))

(define (node-type node)
  (car node))

