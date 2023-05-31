bits 64
default rel

;; =============================================================================
;;  Constant data.
;; =============================================================================
section .rodata
error_usage db "Usage: ./lexer [<filename>]", 0
error_duplicate_filename db "ERROR: Must not specify more than one input file", 10, 0
error_open_read_failed db "ERROR: Could not open file for reading", 0
error_read_failed db "ERROR: Could not read from file", 0
error_integer_overflow db "ERROR: Integer overflow", 10, 0

string_format_read_file_error db "%s: '%s'", 10, 0
string_format_location db "%s at (%u:%u): ", 0
string_format_unexpected_character db "ERROR: Unexpected character U+%hhx ('%c')", 10, 0
string_format_string db "%s", 0
string_format_integer_token_value db " %llu", 10, 0
string_format_ident_token_value db " %.*s", 10, 0
string_format_read_errno db "ERROR: Read failed with errno %d", 10, 0

string_open_mode_read db "rb", 0
string_dot_exit db ".exit", 10, 0
string_default_filename db "<input>", 0
string_default_prompt db ">> ", 0
DEFAULT_PROMPT_SIZE equ $ - string_default_prompt - 1

;; =============================================================================
;;  Static variables.
;; =============================================================================
section .bss
filename resq 1
file_contents resq 1
file_size resq 1

prompt resq 1
prompt_size resq 1

;; State.
has_error resb 1

;; =============================================================================
;;  Includes.
;; =============================================================================
%include "utils.asm"
%include "tokens.asm"
%include "io.asm"
%include "lexer.asm"

;; =============================================================================
;; Register usage:
;; =============================================================================
;; While reading input:
;;   r12: file descriptor
;;   r13: buffer data
;;   r14: buffer size
;;   r15: buffer capacity
;;
;; While lexing/parsing:
;;   rbx: token type
;;   rbp: start of input
;;   r12: current token position (upper 4 bytes = offset, lower 4 bytes = size)
;;   r13: current lexer position
;;   r14: end of input
;;   r15: last character read
;;

;; =============================================================================
;; Entry point.
;; =============================================================================
section .text
global main
main:
    sub rsp, 8 ; Align stack
    call parse_args
    test rax, rax
    jnz .error_usage

    mov rdi, [filename]
    test rdi, rdi
    jz .no_file

    call interpret_file
    mov edi, eax
    jmp exit

.no_file:
    call repl
    mov edi, eax
    jmp exit

.error_usage:
    lea rdi, [error_usage]

.print_exit:
    call puts
    mov edi, 1
    jmp exit

;;
;; Interpret a file.
;;
;; Returns 0 on success, 1 on failure.
interpret_file:
    sub rsp, 8 ; Align stack

    mov rdi, [filename]
    lea rsi, [file_contents]
    lea rdx, [file_size]
    call read_file
    add rsp, 8

    ;; Interpret the code or return error.
    test rax, rax
    jz interpret
    ret

;;
;; Run a REPL.
;;
repl:
    push rbx
    push r12 ; fd
    push r13 ; Buffer data
    push r14 ; Buffer size
    push r15 ; Buffer capacity.

    xor ebx, ebx
    xor r12, r12
    xor r13, r13
    xor r15, r15

.read:
    ;; Write prompt.
    mov eax, SYS_write
    mov rdi, 1 ; fd of stdout == 1
    mov rsi, [prompt]
    mov rdx, [prompt_size]
    syscall

    ;; Read input.
    xor r14, r14
    xor r12, r12 ; fd of stdin == 0
    call read
    test rax, rax
    jnz .error

    ;; Empty line.
    test r14, r14
    jz .read

    ;; ".exit" to exit.
    mov rdx, r14
    cmp rdx, 6 ; strlen(".exit\n") == 6
    jne .eval_print
    lea rdi, [string_dot_exit]
    mov rsi, r13
    call strncmp ; Note: rdx is already 6 if we get here.
    jz .exit_repl

.eval_print:
    saveregs
    call eval
    rstorregs
    test eax, eax
    jnz .error
    jmp .read

.exit_repl:
    xor r15, r15 ; Return value.
    jmp .free_buffer

.error:
    mov r15, 1 ; Return value.

.free_buffer:
    mov rdi, r13
    call free

.return:
    mov rax, r15
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;;
;; Prepare string for evaluation and evaluate it.
;;
eval:
    lea rax, [string_default_filename]
    mov [filename], rax
    mov [file_contents], r13
    mov [file_size], r14
    jmp interpret

;;
;; Interpreter main function.
;;
interpret:
    sub rsp, 8 ; Align stack.
    mov byte [has_error], 0

    ;; Prime the lexer.
    call lexer_init
    next_char

.read_next_token:
    call next_token
    call print_token
    cmp ebx, TK_EOF
    jne .read_next_token

    movsx eax, byte [has_error]
    add rsp, 8
    ret

;;
;; Parse command line args.
;;
;; rsi: u8** argv
;;
parse_args:
    sub rsp, 8 ; Align stack
    add rsi, 8 ; argv+1

    ;; Set default prompt.
    lea rdi, [string_default_prompt]
    mov [prompt], rdi
    mov qword [prompt_size], DEFAULT_PROMPT_SIZE

.parse_one_arg:
    mov rdi, [rsi]
    test rdi, rdi
    jz .last_arg

    mov rdx, [filename]
    test rdx, rdx
    jnz .duplicate_file ; duplicate filename

    mov [filename], rdi
    add rsi, 8
    jmp .parse_one_arg

.duplicate_file:
    lea rdi, [error_duplicate_filename]
    call puts
    mov eax, 1
    jmp .return

.last_arg:
    xor eax, eax

.return:
    add rsp, 8
    ret
