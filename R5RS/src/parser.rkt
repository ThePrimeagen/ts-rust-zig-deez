(load "../src/utils.rkt")
(loader "token")
(loader "lexer")
(loader "ast")

; CONSTS
(define LOWEST 1)
(define EQUALS 2)
(define LESSGREATER 3)
(define SUM 4)
(define PRODUCT 5)
(define PREFIX 6)
(define CALL 7)
(define INDEX 8)

(define precedences (make-hash))
(hash-set! precedences EQ EQUALS)
(hash-set! precedences NOT_EQ EQUALS)
(hash-set! precedences LT LESSGREATER)
(hash-set! precedences GT LESSGREATER)
(hash-set! precedences PLUS SUM)
(hash-set! precedences MINUS SUM)
(hash-set! precedences SLASH PRODUCT)
(hash-set! precedences ASTERISK PRODUCT)
(hash-set! precedences LPAREN CALL)
(hash-set! precedences LBRACKET INDEX)



; PARSER LIST
; (LEXER CUR PEEK STMTS ERRORS prefixHash infixHash)
(define (new-parser l)
  (define parser (list 'parser l ILLEGAL ILLEGAL '() '() (make-hash) (make-hash)))
  (parser-next-token parser)
  (parser-next-token parser)

  (add-prefix! parser IDENT parse-identifier)
  (add-prefix! parser INT parse-integer-literal)
  (add-prefix! parser BANG parse-prefix-exp)
  (add-prefix! parser MINUS parse-prefix-exp)
  (add-prefix! parser TRUE parse-bool-exp)
  (add-prefix! parser FALSE parse-bool-exp)
  (add-prefix! parser LPAREN parse-group-exp)
  (add-prefix! parser IF_ parse-if-exp)
  (add-prefix! parser FUNCTION parse-fn-literal)
  (add-prefix! parser MONKEY_STRING parse-string-literal)
  (add-prefix! parser LBRACKET parse-array-literal)
  (add-prefix! parser LBRACE parse-hash-literal)

  (add-infix! parser PLUS parse-infix-exp)
  (add-infix! parser MINUS parse-infix-exp)
  (add-infix! parser SLASH parse-infix-exp)
  (add-infix! parser ASTERISK parse-infix-exp)
  (add-infix! parser EQ parse-infix-exp)
  (add-infix! parser NOT_EQ parse-infix-exp)
  (add-infix! parser LT parse-infix-exp)
  (add-infix! parser GT parse-infix-exp)
  (add-infix! parser LPAREN parse-call-exp)
  (add-infix! parser LBRACKET parse-index-expression)
  
  parser)

(define (is-parser? p)
  (tagged-list? p 'parser))


; GETTERS
(define (parser-lexer p)
  (cadr p))

(define (parser-cur p)
  (caddr p))

(define (parser-peek p)
  (cadddr p))

(define (parser-stmts p)
  (cadr (cdddr p)))

(define (parser-errors p)
  (caddr (cdddr p)))

(define (parser-prefix p)
  (caddr (cddddr p)))

(define (parser-infix p)
  (car (cdddr (cddddr p))))

(define (get-prefix p key)
  (hash-ref (parser-prefix p) key (lambda () '())))

(define (get-infix p key)
  (hash-ref (parser-infix p) key (lambda () '())))


; SETTERS
(define (parser-set-cur! p t)
  (set-car! (cddr p) t))

(define (parser-set-peek! p t)
  (set-car! (cdddr p) t))

(define (set-parser-stmts! p stmts)
  (set-car! (cdr (cdddr p)) stmts))

(define (set-parser-errors! p stmts)
  (set-car! (cddr (cdddr p)) stmts))

(define (add-prefix! p key value)
  (hash-set! (parser-prefix p) key value))

(define (add-infix! p key value)
  (hash-set! (parser-infix p) key value))


; ADDERS
(define (parser-add-peek-error p t)
  (set-parser-errors! p (add-to-list (parser-errors p) (format "expected next token to be " t " but was " (token-type (parser-peek p))))))

(define (parser-add-int-conv-error p t)
  (set-parser-errors! p (add-to-list (parser-errors p) (format "could not parse as integer " t))))

(define (parser-prefix-parse-error p t)
  (set-parser-errors! p (add-to-list (parser-errors p) (format "no prefix parse function found for " t " with literal:'" (token-literal-as-string (parser-cur p)) "'"))))

(define (add-stmt p stmt)
  (if (not (eq? stmt '()))
      (set-parser-stmts! p (add-to-list (parser-stmts p) stmt))))


; METHODS

(define (parser-cur-is p t)
  (token-is-type? (parser-cur p) t))

(define (parser-peek-is p t)
  (token-is-type? (parser-peek p) t))

(define (parser-expect-peek p t)
  (if (parser-peek-is p t)
      (begin (parser-next-token p) #t)
      (begin (parser-add-peek-error p t) #f)))

(define (parser-next-token p)
  (parser-set-cur! p (parser-peek p))
  (parser-set-peek! p (next-token (parser-lexer p))))

(define (parser-token-literal p)
  (if (> (length p) 0)
      (token-literal-from-state (car p))
      ""))

(define (parser-peek-precedences p)
  (hash-ref precedences (token-type (parser-peek p)) (lambda () LOWEST)))

(define (parser-cur-precedences p)
  (hash-ref precedences (token-type (parser-cur p)) (lambda () LOWEST)))

;; PARSE METHOD

(define (parse-expression p precedence)
  (define prefix (get-prefix p (token-type (parser-cur p))))

  (define (inner left)
    (if (and (not (parser-peek-is p SEMICOLON)) (< precedence (parser-peek-precedences p)))
        (let* ((infix (get-infix p (token-type (parser-peek p))))) (begin (parser-next-token p) (inner (infix p left))))
        left))
 
  (if (null? prefix)
      (begin (parser-prefix-parse-error p (token-type (parser-cur p))) '())
      (inner (prefix p))))

(define (parse-identifier p)
  (new-identifier-node-from-token (parser-cur p)))

(define (parse-integer-literal p)
  (let* ((token (parser-cur p))
  (value (string->number (token-literal token))))
  (if (not value)
      (begin (parser-add-int-conv-error p (token-literal token)) '())
      (new-integeral-literal token value))))

(define (parse-bool-exp p)
  (new-bool (parser-cur p) (parser-cur-is p TRUE)))

(define (parse-group-exp p)
  (parser-next-token p)
  (define exp (parse-expression p LOWEST))
  (if (not (parser-expect-peek p RPAREN)) '() exp))

(define (parse-if-exp p)
  (define token (parser-cur p))
  (if (parser-expect-peek p LPAREN)
      (begin (parser-next-token p)
        (let* ((condition (parse-expression p LOWEST)))
          (if (parser-expect-peek p RPAREN)
              (if (parser-expect-peek p LBRACE)
                  (let* ((cons (parse-block p)))
                    (if (parser-peek-is p ELSE_)
                        (begin (parser-next-token p) (if (parser-expect-peek p LBRACE) (new-if-node token condition cons (parse-block p))'()))
                        (new-if-node token condition cons '()))) '()) '()))) '()))

(define (parse-block p)
  (define block (new-block-stmt (parser-cur p) '()))

  (parser-next-token p)

  (define (inner)
    (if (and (not (parser-cur-is p RBRACE)) (not (parser-cur-is p EOF)))
        (let* ((stmt (parse-stmt p))) (if (not (null? stmt)) (begin (add-stmt-to-block block stmt) (parser-next-token p) (inner))))))
  
  (inner)
  block)

(define (parse-fn-literal p)
  (define token (parser-cur p))
  (if (parser-expect-peek p LPAREN) (let* ((params (parse-fn-params p))) (if (parser-expect-peek p LBRACE) (new-fn token params (parse-block p)) '())) '()))

(define (parse-fn-params p)
  (define identifiers '())
  (define (inner)
    (if (parser-peek-is p COMMA)
        (begin
          (parser-next-token p)
          (parser-next-token p)
          (set! identifiers (add-to-list identifiers (new-identifier-node-from-token (parser-cur p))))
          (inner)
          ))
    )

  (if (parser-peek-is p RPAREN) (begin (parser-next-token p) '())
      (begin
        (parser-next-token p)
        (set! identifiers (add-to-list identifiers (new-identifier-node-from-token (parser-cur p))))
        (inner)
        (if (parser-expect-peek p RPAREN)
            identifiers
            '()))))

(define (parse-string-literal p)
  (new-string (parser-cur p) (token-literal (parser-cur p))))

(define (parse-array-literal p)
  (new-array (parser-cur p) (parse-expression-list p RBRACKET)))

(define (parse-expression-list p end)
  (define exps '())
  (define (inner)
    (if (parser-peek-is p COMMA) (begin (parser-next-token p) (parser-next-token p) (set! exps (add-to-list exps (parse-expression p LOWEST))) (inner))))

  (if (parser-peek-is p end) (begin (parser-next-token p) exps)
      (begin (parser-next-token p) (set! exps (add-to-list exps (parse-expression p LOWEST))) (inner)
             (if (not (parser-expect-peek p end)) '() exps))))

(define (parse-stmt p)
  (define token (parser-cur p))
  (cond
    ((token-is-type? token LET) (parse-let-stmt p))
    ((token-is-type? token RETURN) (parse-return-stmt p))

    (else (parse-expression-stmt p))))

(define (parse-let-stmt p)
  (define stmt '())
  
  (if (not (parser-expect-peek p IDENT))
      (set! stmt '()))

  (set! stmt (new-let-node (new-identifier-node-from-token (parser-cur p))  (new-identifier-node-from-type ILLEGAL)))

  (if (not (parser-expect-peek p ASSIGN))
      (set! stmt '()))

  (parser-next-token p)
  (set-let-value! stmt (parse-expression p LOWEST))
  (if (parser-peek-is p SEMICOLON) (parser-next-token p))
  stmt)

(define (parse-return-stmt p)
  (parser-next-token p)

  (define stmt (new-return-node (parse-expression p LOWEST)))
  
  (define (inner)
    (if (not (parser-cur-is p SEMICOLON))
        (begin (parser-next-token p) (inner))))
  (inner)
  stmt)

(define (parse-expression-stmt p)
  (define exp (new-exp-node (parser-cur p) (parse-expression p LOWEST)))

  (if (parser-peek-is p SEMICOLON)
      (parser-next-token p))
  exp)

(define (parse-prefix-exp p)
  (let* ((token (parser-cur p))
        (operator (token-literal token)))
    (begin (parser-next-token p) (new-prefix-node token operator (parse-expression p PREFIX)))))

(define (parse-infix-exp p left)
  (let* ((token (parser-cur p))
         (operator (token-literal token))
         (precedence (parser-cur-precedences p)))
    (begin (parser-next-token p) (new-infix-node token left operator (parse-expression p precedence)))))

(define (parse-call-exp p fn)
  (new-call-expression (parser-cur p) fn (parse-expression-list p RPAREN)))

(define (parse-index-expression p left)
  (define token (parser-cur p))
  (parser-next-token p)
  (define index (parse-expression p LOWEST))
  (if (not (parser-expect-peek p RBRACKET)) '() (new-index-node token left index)))

(define (parse-hash-literal p)
  (define hash (new-hash-literal (parser-cur p)))
  (define (inner)
    (if (not (parser-peek-is p RBRACE))
        (begin (parser-next-token p)
        (let* ((key (parse-expression p LOWEST)))
          (if (not (parser-expect-peek p COLON)) '()
              (begin (parser-next-token p) (let* ((value (parse-expression p LOWEST))) (hash-literal-set! hash key value))
                     (if (and (not (parser-peek-is p RBRACE)) (not (parser-expect-peek p COMMA))) '() (inner))))))))

  (inner)
  (if (not (parser-expect-peek p RBRACE)) '() hash))
  

; CALL PARSER
(define (parse-programme p)
    (if (not (parser-cur-is p EOF))
        (begin (add-stmt p (parse-stmt p)) (parser-next-token p) (parse-programme p))
        p))