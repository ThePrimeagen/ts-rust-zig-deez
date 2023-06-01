; -----------------------
; Lexer:                  			non packed struct
;  - source: char*        			offset = 0
;  - length: uint64_t     			offset = 8
;  - pos: uint64_t        			offset = 16
;  - ch: char             			offset = 24
; ----------------------

; TokenType
%define TOKEN_EOF          0

%define TOKEN_LSQUIRLY     1			; {
%define TOKEN_RSQUIRLY     2			; }
%define TOKEN_LPAREN       3			; (
%define TOKEN_RPAREN       4			; )

%define TOKEN_PLUS         5			; +
%define TOKEN_DASH         6			; -
%define TOKEN_COMMA        7			; ,
%define TOKEN_SEMICOLON    8			; ;
%define TOKEN_BANG         9			; !
%define TOKEN_ASTERIKS     10			; *
%define TOKEN_ASSIGN       11			; =
%define TOKEN_FORWARDSLASH 12			; /

section .text

; public functions
global lexer_init
global lexer_next

; args:
;  - lexer: Lexer*       			offset = 0
;  - source: char*       			offset = 8
;  - length: uint64_t    			offset = 16
lexer_init:
	mov [rdi], rsi             		; lexer->source = source
	mov [rdi + 8], rdx         		; lexer->length = length
	mov qword [rdi + 16], 0			; lexer->pos    = 0

	ret
	
; args:
;  - lexer: Lexer*
lexer_next:
	call lexer_advance
	call lexer_skipwhitespace

	; compare lexer->ch
	movzx edx, byte [rdi + 24]
	cmp dl, 123                    	; byte code for '{'
	je lexer_case_l_squirly
	cmp dl, 125                    	; byte code for '}'
	je lexer_case_r_squirly

lexer_case_l_squirly:
	mov eax, TOKEN_LSQUIRLY
	jmp end

lexer_case_r_squirly:
	mov eax, TOKEN_RSQUIRLY
	jmp end
end:
	ret

; args:
;  - lexer: Lexer*
lexer_advance:
	mov rax, [rdi + 16]
	xor edx, edx

	cmp rax, [rdi + 8]
	; jump if pos >= length
	jge lexer_advance_write

	; load char
	mov rdx, [rdi]			   		; load the address of source
	movzx edx, byte [rdx + rax]		; set edx to the char of lexer->source[lexer->read]

	; increment pos
	add rax, 1
	mov [rdi + 16], rax
lexer_advance_write:
	mov [rdi + 24], dl
	ret

lexer_skipwhitespace:
	ret