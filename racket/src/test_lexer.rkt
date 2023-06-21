#lang racket
(require rackunit)
(require "lexer.rkt")

;; Test helper
(define (lex-input input expected)
  (check-equal? (init input) expected))

;; Symbols
(lex-input "==" (list 'equal 'eof))
(lex-input "!=" (list 'not-equal 'eof))
(lex-input ">" (list 'greater-than'eof))
(lex-input "+" (list 'plus 'eof))
(lex-input "-" (list 'minus 'eof))
(lex-input ";" (list 'semi-colon 'eof))

;; Identifiers
(lex-input "test" (list (l:identifier "test") 'eof))
(lex-input "return" (list 'return 'eof))
(lex-input "let" (list 'let 'eof))
(lex-input "fn" (list 'fn 'eof))

;; Numbers
(lex-input "123" (list (l:int "123") 'eof))

;; Complete statements
(lex-input "let x = 10;" (list 'let (l:identifier "x") 'assign (l:int "10") 'semi-colon 'eof))
