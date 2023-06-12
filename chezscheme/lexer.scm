(import (chezscheme))

; Wrap effectful get-char in lazy stream
(define (port->stream port)
  (let loop () (delay (let ([c (get-char port)])
    (if (eof-object? c) '() (cons c (loop)))))))

; Lazy stream helper procedures
(define (stream-null? stream) (null? (force stream)))
(define (stream-cons? stream) (not (stream-null? stream)))
(define (stream-head stream) (car (force stream)))
(define (stream-tail stream) (cdr (force stream)))

; Extend ChezScheme with pattern matching syntax
(define-syntax (match/1 code)
  (syntax-case code ()
    [(_) #'(void)]
    [(_ _) #'(void)]
    ; Default else-behavior on missing else clause
    [(_ value clause ks) #'(match/1 value clause ks (void))]

    ; Matching empty stream
    [(_ value (()) ks kf) #'(if (stream-null? value) ks kf)]

    ; Bind rest of stream
    [(_ value (x) ks kf) (identifier? #'x)
      #'(let ([x value]) ks)]

    ; If the else clause is a compound datum, simplify it to avoid explosion of generated code
    [(_ value (x xs ...) ks (kf0 kf1 kfs ...))
      #'(let ([kf (lambda () (kf0 kf1 kfs ...))])
        (match/1 value (x xs ...) ks (kf)))]

    ; Bind head and rest of stream if head passes predicate p
    [(_ value ((p x) xs ...) ks kf) (identifier? #'x)
      #'(if (and (stream-cons? value) (p (stream-head value)))
        (let ([x (stream-head value)]
              [tail (stream-tail value)])
          (match/1 tail (xs ...) ks kf))
          kf)]

    ; Bind head and rest of stream
    [(_ value (x xs ...) ks kf) (identifier? #'x)
      #'(if (stream-cons? value)
        (let ([x (stream-head value)]
              [tail (stream-tail value)])
          (match/1 tail (xs ...) ks kf)))]

    ; Matching on char literal
    [(_ value (x xs ...) ks kf) (char? (syntax->datum #'x))
      #'(if (and (stream-cons? value) (eq? (stream-head value) x))
        (let ([tail (stream-tail value)])
          (match/1 tail (xs ...) ks kf))
        kf)]
    
    ; Matching on string literal
    [(_ value (x xs ...) ks kf) (string? (syntax->datum #'x))
      #`(match/1 value (#,@(string->list (syntax->datum #'x)) xs ...) ks kf)]))

; Problem solution:

(define (take-while ss p?)
  (let loop ([acc '()] [ss ss])
    (match/1 ss ((p? x) rest)
      (loop (cons x acc) rest)
      (values (reverse acc) ss))))

(define (parse-digit* ss k)
  (let-values ([(numbers rest) (take-while ss char-numeric?)])
    (and (not (null? numbers)) (k (string->number (list->string numbers)) rest))))

(define (parse-letter* ss k)
  (let-values ([(letters rest) (take-while ss char-alphabetic?)])
    (and (not (null? letters)) (k (list->string letters) rest))))

(define (parse-whitespace* ss k)
  (let-values ([(space rest) (take-while ss char-whitespace?)])
    (and (not (null? space)) (k (list->string space) rest))))

(define (lexer ss)
  (call/1cc (lambda (k)
    (parse-whitespace* ss (lambda (result ss) (k 'Space ss)))
    (parse-digit* ss (lambda (result ss) (k (cons 'Int result) ss)))
    (match/1 ss ("let" ss) (k 'Let ss))
    (match/1 ss ("fn" ss) (k 'Function ss))
    (parse-letter* ss (lambda (result ss) (k (cons 'Ident result) ss)))
    (match/1 ss ("=" ss) (k 'Equal ss))
    (match/1 ss (";" ss) (k 'SemiColon ss))
    (match/1 ss ("," ss) (k 'Comma ss))
    (match/1 ss ("(" ss) (k 'LParen ss))
    (match/1 ss (")" ss) (k 'RParen ss))
    (match/1 ss ("{" ss) (k 'LSquirly ss))
    (match/1 ss ("}" ss) (k 'RSquirly ss))
    (match/1 ss ("+" ss) (k 'Plus ss))
    (match/1 ss (_ ss) (k 'Illegal ss))
    (match/1 ss (()) (k 'Eof ss))
    #f)))

(let ([ss (port->stream (current-input-port))])
  (let loop ([ss ss])
    (let-values ([(token ss) (lexer ss)])
      (unless (eq? token 'Space)
        (if (symbol? token)
            (format #t "~a\n" token)
            (format #t "~a ~a\n" (car token) (cdr token))))
      (unless (eq? token 'Eof) (loop ss)))))
