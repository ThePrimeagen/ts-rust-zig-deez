(load "../src/utils.rkt")
(loader "token")


; LET STATEMENT
; (token 'identifier-state EXP)
(define (new-let-state id value)
  (list 'let-state (to-token LET "let") id value))

(define (let-stmt? s)
  (tagged-list? s 'let-state))

(define (set-let-value! l value)
  (if (not (null? l)) (set-car! (cdddr l) value)))

(define (let-value l)
  (cadddr l))

(define (let-id stmt)
  (caddr stmt))


; IDENTENTIFIER STATEMENT
; (token value)
(define (new-identifier token value)
  (list 'identifier-state token value))

(define (new-identifier-from-token token)
  (new-identifier token (token-literal token)))

(define (new-identifier-from-type type)
  (new-identifier-from-token (token-from-type type)))

(define (id-stmt? s)
  (tagged-list? s 'identifier-state))

(define (id-value id)
  (caddr id))

(define (id-token id)
  (cadr id))


; RETURN STATEMENT
; (RETURN EXP)
(define (new-return-state exp)
  (list 'return-state (token-from-type RETURN) exp))

(define (return-stmt? s)
  (tagged-list? s 'return-state))

(define (return-value s)
  (caddr s))


; EXPRESSION STATEMENT
; (token exp)
(define (new-expression-state first-token value)
  (list 'expression-state first-token value))

(define (expression-stmt? s)
  (tagged-list? s 'expression-state))

(define (expression-value s)
  (caddr s))

(define (expression-token s)
  (cadr s))


;; PREFIX EXPRESSION
; (token operator right)
(define (new-prefix-expression token operator right)
  (list 'prefix-expression token operator right))

(define (prefix-exp? s)
  (tagged-list? s 'prefix-expression))

(define (prefix-exp-right exp)
  (car (cdddr exp)))

(define (prefix-exp-operator exp)
  (caddr exp))

(define (prefix-exp-token s)
  (cadr s))


; INFIX EXPRESSIONS
; (token left operator right)
(define (new-infix-expressions token left operator right)
  (list 'infix-expression token left operator right))

(define (infix-exp? s)
  (tagged-list? s 'infix-expression))

(define (infix-exp-right exp)
  (cadr (cdddr exp)))

(define (infix-exp-operator exp)
  (car (cdddr exp)))

(define (infix-exp-left exp)
  (caddr exp))

(define (infix-exp-token s)
  (cadr s))


; INTEGER LITERAL
; (token value)
(define (new-integeral-literal token value)
  (list 'integeral-literal token value))

(define (int-literal? s)
  (tagged-list? s 'integeral-literal))

(define (int-value literal)
  (caddr literal))

(define (int-token s)
  (cadr s))


; BOOLEAN
; (token value)
(define (new-bool token value)
  (list 'bool token value))

(define (bool-literal? s)
  (tagged-list? s 'bool))

(define (bool-value literal)
  (caddr literal))

(define (bool-token s)
  (cadr s))


; IF EXPRESSION
; (token condition consequence alternative)
(define (new-if-exp token condition consequence alternative)
  (list 'if-exp token condition consequence alternative))

(define (if-exp? s)
  (tagged-list? s 'if-exp))

(define (if-token s)
  (cadr s))

(define (if-cond s)
  (caddr s))

(define (if-cons s)
  (cadddr s))

(define (if-alt s)
  (cadr (cdddr s)))


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

