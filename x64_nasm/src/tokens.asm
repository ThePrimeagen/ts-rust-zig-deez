%line 1 "tokens.asm"
;; =============================================================================
;;  Token types.
;; =============================================================================
TK_INVALID   equ 0
TK_EOF       equ 1
TK_IDENT     equ 2
TK_INTEGER   equ 3
TK_ASSIGN    equ 4
TK_PLUS      equ 5
TK_COMMA     equ 6
TK_SEMICOLON equ 7
TK_LPAREN    equ 8
TK_RPAREN    equ 9
TK_LBRACE    equ 10
TK_RBRACE    equ 11
TK_EQ_EQ     equ 12
TK_NOT       equ 13
TK_NE        equ 14
TK_MINUS     equ 15
TK_SLASH     equ 16
TK_STAR      equ 17
TK_LT        equ 18
TK_GT        equ 19

KW_START     equ 20
TK_FUNCTION  equ KW_START
TK_LET       equ KW_START + 1
TK_RETURN    equ KW_START + 2
TK_TRUE      equ KW_START + 3
TK_FALSE     equ KW_START + 4
TK_IF        equ KW_START + 5
TK_ELSE      equ KW_START + 6
