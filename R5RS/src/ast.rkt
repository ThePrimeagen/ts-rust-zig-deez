(load "../src/utils.rkt")
(loader "token")

; LET STATEMENT
; (token 'identifier-state EXP)
(define (new-let-state id value)
  (list 'let-state (to-token LET "let") id value))

(define (let-stmt? s)
  (tagged-list? s 'let-state))

(define (set-let-value! l value)
  (set-car! (cdddr l) value))

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


; SHARED METHODS
(define (token-literal-from-state state)
  (token-literal (cadr state)))

