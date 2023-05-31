(load "token.rkt")
(load "utils.rkt")


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
; (token operator exp)
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


; SHARED METHODS
(define (token-literal-from-state state)
  (token-literal (cadr state)))

