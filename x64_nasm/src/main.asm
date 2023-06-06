bits 64
default rel

;; =============================================================================
;;  Includes.
;; =============================================================================
%use smartalign

%include "sys.asm"
%include "utils.asm"
%include "tokens.asm"
%include "io.asm"
%include "lexer.asm"
%include "ast.asm"
%include "parser.asm"
%line 15 "main.asm"

;; =============================================================================
;; Register usage:
;; =============================================================================
;; While reading input from the REPL:
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
;;  Context
;; =============================================================================
struc context
    .__gs resq 1
    .__oldgs resq 1
    .filename resq 1
    .file_contents resq 1
    .file_size resq 1
    .argv resq 1

    .prompt resq 1
    .prompt_size resq 1

    .has_error resb 1
endstruc

;; =============================================================================
;;  Memory management.
;; =============================================================================
%define PAGESIZE 4096

mmap_page:
    sub rsp, 8 ; Align stack
    mov eax, SYS_mmap
    xor edi, edi
    mov esi, PAGESIZE
    mov edx, PROT_READ | PROT_WRITE
    mov r10, MAP_PRIVATE | MAP_ANONYMOUS
    mov r8d, -1 ; Should be -1 if MAP_ANONYMOUS is used.
    xor r9d, r9d
    syscall
    add rsp, 8
    ret

;; =============================================================================
;;  Process initialisation.
;; =============================================================================
init:
    push rsi ; Save argv and align stack.
    push r15

    ;; Allocate context.
    call mmap_page
    mov r15, rax

    ;; Get and save old GS value at GS:8.
    sub rsp, 8
    mov eax, SYS_arch_prctl
    mov edi, ARCH_GET_GS
    mov rsi, rsp
    syscall
    mov rdx, [rsp]
    mov [r15 + context.__oldgs], rdx
    add rsp, 8
    test rax, rax
    jnz .error

    ;; Initialise GS.
    mov eax, SYS_arch_prctl
    mov edi, ARCH_SET_GS
    mov rsi, r15
    syscall
    test rax, rax
    jnz .error

    ;; Save the address of GS:0 at GS:0.
    mov [r15 + context.__gs], r15
    pop r15
    pop rsi

    ;; Save argv.
    mov [gs:context.argv], rsi
    ret

.error:
    pop r15
    lea rdi, [error_arch_prctl_failed]
    call puts
    mov edi, 1
    call exit

;; =============================================================================
;;  Entry point.
;; =============================================================================
section .text
global main
main:
    sub rsp, 8 ; Align stack
    call init
    call parse_args
    test rax, rax
    jnz .error_usage

    mov rdi, [gs:context.filename]
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

    mov rdi, [gs:context.filename]
    mov rax, [gs:0]
    lea rsi, [rax + context.file_contents] ; Note LEA + GS don’t play well together...
    lea rdx, [rax + context.file_size]
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
    mov rdi, STDOUT_FILENO
    mov rsi, [gs:context.prompt]
    mov rdx, [gs:context.prompt_size]
    syscall

    ;; Read input.
    xor r14, r14
    mov r12, STDIN_FILENO
    mov edi, 1 ; Reading from tty.
    call read
    test rax, rax
    jnz .error

    ;; Empty line.
    test r14, r14
    jz .handle_empty_line

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

.handle_empty_line:
    ;; Read next line only if EOF hasn’t been reached. read()
    ;; uses rdx to tell us whether we’re at EOF or not.
    test rdx, rdx
    jz .read

    ;; Print a newline before exiting.
    mov edi, 10
    call putchar

.exit_repl:
    xor r15, r15 ; Return value.
    jmp .free_buffer

.error:
    mov r15, 1 ; Return value.

.free_buffer:
    mov rdi, r13
    call free

.return:
    add rsp, 16
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
    mov [gs:context.filename], rax
    mov [gs:context.file_contents], r13
    mov [gs:context.file_size], r14
    jmp interpret

;;
;; Interpreter main function.
;;
interpret:
    sub rsp, 8 ; Align stack.
    mov byte [gs:context.has_error], 0

    ;; Prime the lexer.
    call lexer_init
    next_char

.read_next_token:
    call next_token
    call print_token
    cmp ebx, TK_EOF
    jne .read_next_token

    movsx eax, byte [gs:context.has_error]
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
    mov [gs:context.prompt], rdi
    mov qword [gs:context.prompt_size], DEFAULT_PROMPT_SIZE

.parse_one_arg:
    mov rdi, [rsi]
    test rdi, rdi
    jz .last_arg

    mov rdx, [gs:context.filename]
    test rdx, rdx
    jnz .duplicate_file ; duplicate filename

    mov [gs:context.filename], rdi
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
