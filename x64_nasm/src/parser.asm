%line 1 "parser.asm"
;; =============================================================================
;;  Parser.
;; =============================================================================
;;
;; Collected grammar:
;;
;; <stmt-let> := "let" <identifier> "=" <expression> ";"
;;
;;
section .text

;;
;; Parse a "let" statement.
;;
;; "let" <identifier> "=" <expression> ";"
;;