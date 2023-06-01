(#%require (only racket/base read-line))
(load "../src/utils.rkt")
(loader "lexer")
(loader "token")
(loader "parser")
(loader "eval")

(define input-prompt ";;; Monkey-Eval input:")
(define output-prompt ";;; Monkey-Eval value:")

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (display-parse-errors p)
  (if (not (= 0 (length (parser-errors p)))) (for-each (lambda (error) (display-l error)) (parser-errors p))))

(define (start)
  (prompt-for-input input-prompt)
  (define input (read-line))
  (let*
      ((lexer (new-lexer input))
       (parser (new-parser lexer))
       (p (parse-programme parser)))
    (begin (parser-errors p) (display-parse-errors p) (let* ((evaluated (monkey-eval p))) (display-l (inspect evaluated)))))
  (start))

(start)
  