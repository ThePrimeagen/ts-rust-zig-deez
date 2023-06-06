global main

extern puts
extern printf

; import lexer
extern lexer_init
extern lexer_next

section .data
print_token_id_fmt:
	db "tok: %d", 10, 0
test_string:
	db "{}", 0

section .text
main:
	push rbp
	mov rbp, rsp

	; reserve space for the lexer
	sub rsp, 32
	lea rdi, [rbp - 32]
	mov rsi, test_string
	mov rdx, 2
	call lexer_init

	call lexer_next
	mov esi, eax
	mov rdi, print_token_id_fmt
	mov eax, 0
	call printf

	lea rdi, [rbp - 32]
	call lexer_next
	mov esi, eax
	mov rdi, print_token_id_fmt
	mov eax, 0
	call printf

	leave
	xor eax, eax
	ret