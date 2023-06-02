(load "../src/utils.rkt")
(loader "lexer")

; TYPES
(define ILLEGAL "ILLEGAL")
(define EOF "EOF")

(define IDENT "IDENT")
(define INT "INT")

(define ASSIGN "=")
(define PLUS "+")
(define MINUS "-")
(define BANG "!")
(define ASTERISK "*")
(define SLASH "/")
(define LT "<")
(define GT ">")
(define EQ "==")
(define NOT_EQ "!=")


(define COMMA ",")
(define SEMICOLON ";")

(define LPAREN "(")
(define RPAREN ")")
(define LBRACE "{")
(define RBRACE "}")

(define FUNCTION "FUNCTION")
(define LET "LET")
(define TRUE "TRUE")
(define FALSE "FALSE")
(define IF_ "IF")
(define ELSE_ "ELSE")
(define RETURN "RETURN")


; TOKEN
; (TYPE literal)
(define (to-token type literal)
    (list 'token type literal)
)

(define (token-from-type type)
  (to-token type type))

(define (token? t)
  (tagged-list? t 'token))

; GETTERS
(define (token-type token)
    (if (token? token)
    (cadr token)))

(define (token-literal token)
    (if (token? token)
        (caddr token)))

(define (token-literal-as-string token)
  (define lit (token-literal token))
  (cond
    ((number? lit) (string (integer->char lit)))
    ((string? lit) lit)
    (else "IDK what to do with this")))



; EQ?
(define (token-is-type? token type)
  (string=? type (token-type token)))

(define (char-eq? ch token)
  (if (number? ch)
  (cond
    ((number? token) (eq? ch token))
    ((= 0 (string-length token)) (eq? ch 0))
    (else (eq? ch (char->integer (string-ref token 0)))))
  (string=? ch token)))


; KEYWORD?
(define (lookup-ident id)
  (cond
    ((string=? "fn" id) (to-token FUNCTION id))
    ((string=? "let" id) (to-token LET id))
    ((string=? "true" id) (to-token TRUE id))
    ((string=? "false" id) (to-token FALSE id))
    ((string=? "if" id) (to-token IF_ id))
    ((string=? "else" id) (to-token ELSE_ id))
    ((string=? "return" id) (to-token RETURN id))
    (else (to-token IDENT id))))



; LEXER BACK BONE
; (lexer)
(define (next-token l)
  (lexer-skip-whitespace l)

  (define ch (lexer-char l))
  (define out (to-token ILLEGAL ""))
  
  (cond
    ((char-eq? ch ASSIGN)
     (if (char-eq? (lexer-peak-char l) "=")
         (begin
           (lexer-read-char l)
           (set! out (to-token EQ (string-append (string (integer->char ch)) (string (integer->char (lexer-char l))))))
           (lexer-read-char l)
          ) 
         (begin (lexer-read-char l) (set! out (to-token ASSIGN ch))))
         out)
    
    ((char-eq? ch SEMICOLON) (begin (lexer-read-char l) (to-token SEMICOLON ch)))
    ((char-eq? ch LPAREN) (begin (lexer-read-char l) (to-token LPAREN ch)))
    ((char-eq? ch RPAREN) (begin (lexer-read-char l) (to-token RPAREN ch)))
    ((char-eq? ch LBRACE) (begin (lexer-read-char l) (to-token LBRACE ch)))
    ((char-eq? ch RBRACE) (begin (lexer-read-char l) (to-token RBRACE ch)))
    
    ((char-eq? ch COMMA) (begin (lexer-read-char l) (to-token COMMA ch)))
    ((char-eq? ch PLUS) (begin (lexer-read-char l) (to-token PLUS ch)))
    ((char-eq? ch MINUS) (begin (lexer-read-char l) (to-token MINUS ch)))
    
    ((char-eq? ch BANG)
     (if (char-eq? (lexer-peak-char l) "=")
         (begin
           (lexer-read-char l)
           (set! out (to-token NOT_EQ (string-append (string (integer->char ch)) (string (integer->char (lexer-char l))))))
           (lexer-read-char l)
          ) 
         (begin (lexer-read-char l) (set! out (to-token BANG ch))))
         out)
    
    ((char-eq? ch SLASH) (begin (lexer-read-char l) (to-token SLASH ch)))
    ((char-eq? ch ASTERISK) (begin (lexer-read-char l) (to-token ASTERISK ch)))
    ((char-eq? ch LT) (begin (lexer-read-char l) (to-token LT ch)))
    ((char-eq? ch GT) (begin (lexer-read-char l) (to-token GT ch)))
    
    ((char-eq? ch 0) (begin (lexer-read-char l) (to-token EOF ch)))
    
    ((letter? ch) (lookup-ident (lexer-read-identifier l)))
    ((digit? ch) (to-token INT (lexer-read-integer l)))

    (else (begin (lexer-read-char l) (to-token ILLEGAL ch)))
    )
  )



  
    