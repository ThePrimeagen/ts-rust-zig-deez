(#%require (only racket/base read-line))
(load "../src/utils.rkt")
(loader "lexer")
(loader "token")

(define input-prompt ";;; Monkey-Eval input:")
(define output-prompt ";;; Monkey-Eval value:")

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))


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


(repl-loop)
  