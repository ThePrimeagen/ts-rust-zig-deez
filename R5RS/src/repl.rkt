(#%require (only racket/base read-line))
(load "../src/utils.rkt")
(loader "lexer")
(loader "token")
(loader "parser")

(define input-prompt ";;; Monkey-Eval input:")
(define output-prompt ";;; Monkey-Eval value:")

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (display-parse-errors p)
  (if (not (= 0 (length (parser-errors p)))) (for-each (lambda (error) (display-l error)) (parser-errors p))))


(define (monkey-eval input)
  (define l (new-lexer input))

  (define (inner l)
    (define t (next-token l))
    (if (not (eq? (token-type t) EOF))
        (begin (display t) (newline) (inner l))
        (begin (display t) (newline))))
  (display output-prompt)
  (newline)
  (inner l)
  )

(define (repl-loop)
  (prompt-for-input input-prompt)
  (define input (read-line))
  (monkey-eval input)
  (repl-loop))


(define (rppl-loop)
  (prompt-for-input input-prompt)
  (define input (read-line))
  (define p (parse-programme (new-parser (new-lexer input))))
  (display-parse-errors p)
  (display output-prompt)
  (newline)
  (rppl-loop))
  
;(repl-loop)
;(rppl-loop)
  