; ============================================================================ ;
; Lexer:                  			non packed struct
;  - source: char*        			offset = 0
;  - length: uint64_t     			offset = 8
;  - pos: uint64_t        			offset = 16
;  - ch: char             			offset = 24
; ============================================================================ ;

; ============================================================================ ;
; TokenType
; ============================================================================ ;
%define TOKEN_EOF          0
%define TOKEN_ILLEGAL      1
%define TOKEN_IDENT        2

%define TOKEN_LSQUIRLY     3        ; {                           byte code: 123
%define TOKEN_RSQUIRLY     4        ; }                           byte code: 125
%define TOKEN_LPAREN       5        ; (                           byte code: 40
%define TOKEN_RPAREN       6        ; )                           byte code: 41

%define TOKEN_PLUS         7        ; +                           byte code: 43
%define TOKEN_DASH         8        ; -                           byte code: 45
%define TOKEN_COMMA        9        ; ,                           byte code: 44
%define TOKEN_SEMICOLON    10       ; ;                           byte code: 59
%define TOKEN_BANG         11       ; !                           byte code: 33
%define TOKEN_ASTERIKS     12       ; *                           byte code: 42
%define TOKEN_ASSIGN       13       ; =                           byte code: 61
%define TOKEN_FORWARDSLASH 14       ; /                           byte code: 47
%define TOKEN_LESS         15       ; <                           byte code: 60
%define TOKEN_GREATER      16       ; >                           byte code: 63

%define TOKEN_EQUAL        17       ; ==
%define TOKEN_NOTEQUAL     18       ; !=

%define TOKEN_LET          19       ; let
%define TOKEN_IF           20       ; if
%define TOKEN_ELSE         21       ; else
%define TOKEN_RETURN       22       ; return
%define TOKEN_FALSE        23       ; false
%define TOKEN_TRUE         24       ; true

section .text

; ============================================================================ ;
; public functions
; ============================================================================ ;
global lexer_init
global lexer_next

; ============================================================================ ;
; args:
;  - lexer: Lexer*                  offset = 0
;  - source: char*                  offset = 8
;  - length: uint64_t               offset = 16
; ============================================================================ ;
lexer_init:
	mov [rdi], rsi                  ; lexer->source = source
	mov [rdi + 8], rdx              ; lexer->length = length
	mov qword [rdi + 16], 0         ; lexer->pos    = 0

	ret
	
; ============================================================================ ;
; args:
;  - lexer: Lexer*
; ============================================================================ ;
lexer_next:
	call lexer_advance
	call lexer_skipwhitespace

	; compare lexer->ch
	movzx edx, byte [rdi + 24]
	cmp dl, 123                     ; byte code for '{'
	je lexer_case_l_squirly
	cmp dl, 125                     ; byte code for '}'
	je lexer_case_r_squirly

lexer_case_l_squirly:
	mov eax, TOKEN_LSQUIRLY
	jmp end

lexer_case_r_squirly:
	mov eax, TOKEN_RSQUIRLY
	jmp end

end:
	ret

; ============================================================================ ;
; args:
;  - lexer: Lexer*
; ============================================================================ ;
lexer_advance:
	mov rax, [rdi + 16]             ; load the reference of position into rax
	xor edx, edx                    ; set edx to 0

	cmp rax, [rdi + 8]
	jge lexer_advance_write         ; jump if pos >= length

	mov rdx, [rdi]                  ; load the address of source into rdx
	movzx edx, byte [rdx + rax]     ; set edx to the char of lexer->source[lexer->read]

	add rax, 1                      ; increment pos
	mov [rdi + 16], rax 

lexer_advance_write:
	mov [rdi + 24], dl              ; load the value of dl into ch. dl is part of edx thus beeing 0,
	                                ; if not overwritten. This happens if pos >= length
	ret

; ============================================================================ ;
; args:
;  - lexer: Lexer*
;
; Whitespace:
;  - ' '    byte code: 32
;  - '\t'   byte code: 9
;  - '\r'   byte code: 13
;  - '\n'   byte code: 10
; ============================================================================ ;
lexer_skipwhitespace:
	ret