(load "token.rkt")
(load "lexer.rkt")
(load "utils.rkt")

; CONSTS
(define LOWEST 1)
(define EQUALS 2)
(define LESSGREATER 3)
(define SUM 4)
(define PRODUCT 5)
(define PREFIX 6)
(define CALL 7)



; PARSER LIST
; (LEXER CUR PEEK STMTS ERRORS prefixHash infixHash)
(define (new-parser l)
  (define parser (list 'parser l ILLEGAL ILLEGAL '() '() (make-hash) 'indix))
  (parser-next-token parser)
  (parser-next-token parser)

  (add-prefix! parser IDENT (lambda (p) (parse-identifier p)))
  (add-prefix! parser INT (lambda (p) (parse-integer-literal p)))
  (add-prefix! parser BANG (lambda (p) (parse-prefix-exp p)))
  (add-prefix! parser MINUS (lambda (p) (parse-prefix-exp p)))

  
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

(define (get-infex p key)
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
  (set-parser-errors! p (add-to-list (parser-errors p) (format "no prefix parse function found for " t))))

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
  (parser-set-peek! p (next-token (parser-lexer p)))
  )

(define (parser-token-literal p)
  (if (> (length p) 0)
      (token-literal-from-state (car p))
      ""))

(define (parse-expression p precedence)
  (define prefix (get-prefix p (token-type (parser-cur p))))
  (if (eq? prefix '())
      (begin (parser-prefix-parse-error p (token-type (parser-cur p))) '())
      (prefix p)))

(define (parse-identifier p)
  (new-identifier-from-token (parser-cur p)))

(define (parse-integer-literal p)
  (let* ((token (parser-cur p))
  (value (string->number (token-literal token))))
  (if (not value)
      (begin (parser-add-int-conv-error p (token-literal token)) '())
      (new-integeral-literal token value))
  ))
  


; PARSER METHODS
(define (parse-stmt p)
  (define token (parser-cur p))
  (cond
    ((token-is-type? token LET) (parse-let-stmt p))
    ((token-is-type? token RETURN) (parse-return-stmt p))

    (else (parse-expression-stmt p))
    )
  )

(define (parse-let-stmt p)
  (define stmt '())
  
  (if (not (parser-expect-peek p IDENT))
      (set! stmt '()))

  (set! stmt (new-let-state (new-identifier-from-token (parser-cur p))  (new-identifier-from-type ILLEGAL)))

  (if (not (parser-expect-peek p ASSIGN))
      (set! stmt '()))

  (define (inner)
    (if (not (parser-cur-is p SEMICOLON))
        (begin (parser-next-token p) (inner))))

  (inner)
  stmt
  )

(define (parse-return-stmt p)
  (define stmt (new-return-state '())) ;;EXP???

  (parser-next-token p)
  (define (inner)
    (if (not (parser-cur-is p SEMICOLON))
        (begin (parser-next-token p) (inner))))

  (inner)
  stmt
  )

(define (parse-expression-stmt p)
  (define exp (new-expression-state (parser-cur p) (parse-expression p LOWEST)))

  (if (parser-peek-is p SEMICOLON)
      (parser-next-token p) )

  exp
  )

(define (parse-prefix-exp p)
  (let* (
        (token (parser-cur p))
        (operator (token-literal token))
        )
    (begin (parser-next-token p) (new-prefix-expression token operator (parse-expression p PREFIX)))
  ))
  

(define (parse-programme p)
  (define (inner)
    (if (not (parser-cur-is p EOF))
        (begin (add-stmt p (parse-stmt p)) (parser-next-token p) (inner))
    ))

  (inner)
  p
  )