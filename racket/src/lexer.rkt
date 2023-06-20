#lang racket

;; Data types
(struct l:int (value) #:transparent)
(struct l:identifier (value) #:transparent)

;; Helpers
(define (whitespace? s)
  (for/and ([c (in-string s)])
    (char-whitespace? c)))

(define (numeric? s) (number? (string->number s)))

(define (letter? s)
  (cond
    [(eq? 0 (string-length s)) #f]
    [else (char-alphabetic? (car (string->list s)))]))

(define (reverse-and-join str)
  (string-join (reverse str) ""))

;; Entry point
(define (init input)
  (lex (string-split input "") (list)))

;; Lexer
(define (lex input tokens)
  (match input
    ;; EOF
    [(list "") (reverse (cons 'eof tokens))]

    ;; Ignore Whitespace
    [(list x xs ...) #:when (whitespace? x) (lex xs tokens)]

    ;; Symbols
    [(list "=" "=" xs ...)      (lex xs (cons 'equal tokens))]
    [(list "!" "=" xs ...)      (lex xs (cons 'not-equal tokens))]
    [(list ";" xs ...)          (lex xs (cons 'semi-colon tokens))]
    [(list "," xs ...)          (lex xs (cons 'comma tokens))]
    [(list "(" xs ...)          (lex xs (cons 'lparen tokens))]
    [(list ")" xs ...)          (lex xs (cons 'rparen tokens))]
    [(list "{" xs ...)          (lex xs (cons 'lsquirly tokens))]
    [(list "}" xs ...)          (lex xs (cons 'rsquirly tokens))]
    [(list "=" xs ...)          (lex xs (cons 'assign tokens))]
    [(list "+" xs ...)          (lex xs (cons 'plus tokens))]
    [(list "-" xs ...)          (lex xs (cons 'minus tokens))]
    [(list "!" xs ...)          (lex xs (cons 'bang tokens))]
    [(list "/" xs ...)          (lex xs (cons 'slash tokens))]
    [(list "*" xs ...)          (lex xs (cons 'asterisk tokens))]
    [(list ">" xs ...)          (lex xs (cons 'greater-than tokens))]
    [(list "<" xs ...)          (lex xs (cons 'less-than tokens))]
    [(list "<" xs ...)          (lex xs (cons 'less-than tokens))]

    ;; Identifiers
    [(list x xs ...) #:when (letter? x)
                         (define-values (rest identifier) (lex-identifier xs (list x)))
                         (lex rest (cons identifier tokens))]

    ;; Numbers
    [(list x xs ...) #:when (numeric? x)
                         (define-values (rest num) (lex-number xs (list x)))
                         (lex rest (cons num tokens))]))

(define (lex-identifier input identifier)
  (match input
    [(list x xs ...) #:when (letter? x)     (lex-identifier xs (cons x identifier))]
    [xs                                     (values xs (tokenize-identifier (reverse-and-join identifier)))]))

(define (lex-number input number)
  (match input
    [(list x xs ...) #:when (numeric? x)    (lex-number xs (cons x number))]
    [xs                                     (values xs (l:int (reverse-and-join number)))]))

(define (tokenize-identifier identifier)
  (match identifier
    ["fn" 'fn]
    ["let" 'let]
    ["if" 'if]
    ["else" 'else]
    ["true" 'true]
    ["false" 'false]
    ["return" 'return]
    [x (l:identifier x)]))

;; (init "fn f x = x * x")
