(#%require (only racket/base read-line))
(load "../src/utils.rkt")
(loader "lexer")
(loader "token")
(loader "parser")
(loader "eval")
(loader "environment")

(define input-prompt ">>")

(define (prompt-for-input string)
  (newline) (display string))

(define (display-parse-errors p)
  (if (not (= 0 (length (parser-errors p)))) (for-each (lambda (error) (display-l error)) (parser-errors p))))

(define (repl env)
  (prompt-for-input input-prompt)
  (define input (read-line))
  (let*
      ((lexer (new-lexer input))
       (parser (new-parser lexer))
       (p (parse-programme parser)))
    (begin (parser-errors p) (display-parse-errors p) (let* ((evaluated (monkey-eval p env))) (if (not (obj-null? evaluated)) (display-l (inspect evaluated))))))
  (repl env))

(define (start)
  (define env (new-environment))
  (repl env))

(start)
  