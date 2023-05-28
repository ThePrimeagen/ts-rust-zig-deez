bits 64
default rel

;;
;; Constant data.
;;
section .rodata
error_usage db "Usage: ./lexer [<filename>]", 0
error_duplicate_filename db "ERROR: Must not specify more than one input file", 10, 0
error_open_read_failed db "ERROR: Could not open file for reading", 0
error_read_failed db "ERROR: Could not read from file", 0
error_integer_overflow db "ERROR: Integer overflow", 10, 0

string_format_read_file_error db "%s: '%s'", 10, 0
string_format_location db "%s at (%u:%u): ", 0
string_format_unexpected_character db "ERROR: Unexpected character \x%x ('%c')", 10, 0
string_format_string db "%s", 0
string_format_integer_token_value db " %llu", 10, 0
string_format_ident_token_value db " %.*s", 10, 0
string_format_read_errno db "ERROR: Read failed with errno %d", 10, 0

string_open_mode_read db "rb", 0
string_dot_exit db ".exit", 10, 0
string_default_filename db "<input>", 0
string_default_prompt db ">> ", 0
DEFAULT_PROMPT_SIZE equ $ - string_default_prompt - 1

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

%define SYS_read 0
%define SYS_write 1
%define SYS_open 2
%define SYS_close 3

%define KW_START 20
TK_FUNCTION  equ KW_START
TK_LET       equ KW_START + 1
TK_RETURN    equ KW_START + 2
TK_TRUE      equ KW_START + 3
TK_FALSE     equ KW_START + 4
TK_IF        equ KW_START + 5
TK_ELSE      equ KW_START + 6

%define TOKEN_STRING_LENGTH 16     ; Max len of token name is 16 bytes
%define TOKEN_STRING_LENGTH_LOG2 4 ; log2(TOKEN_STRING_LENGTH)

;; LUT for converting token types to strings.
;; Note: Implicitly zero-terminated by the alignment code.
%macro defstr 1
    db %1
    align TOKEN_STRING_LENGTH,db 0
%endmacro

align TOKEN_STRING_LENGTH,db 0
table_token_names:
    defstr "<invalid token>"
    defstr "<end of file>"
    defstr "<identifier>"
    defstr "<int literal>"
    defstr "="
    defstr "+"
    defstr ","
    defstr ";"
    defstr "("
    defstr ")"
    defstr "{"
    defstr "}"
    defstr "=="
    defstr "!"
    defstr "!="
    defstr "-"
    defstr "/"
    defstr "*"
    defstr "<"
    defstr ">"
table_keywords:
    defstr "fn"
    defstr "let"
    defstr "return"
    defstr "true"
    defstr "false"
    defstr "if"
    defstr "else"
table_keywords_end:

%unmacro defstr 1

;;
;; Static variables.
;;
section .bss
filename resq 1
file_contents resq 1
file_size resq 1

prompt resq 1
prompt_size resq 1

;; Lexer state.
lastc resb 1
has_error resb 1
lexer_curr resq 1
lexer_end resq 1
token_type resq 1
token_int_value resq 1
token_location resd 2 ; First dword is start, second dword is end.

;;
;; External functions.
;;
section .text
extern putchar
extern puts
extern printf
extern strncmp
extern exit
extern abort
extern realloc
extern free

;;
;; Entry point.
;;
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
    call eval
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
    call next_char

.read_next_token:
    call next_token
    call print_token
    cmp qword [token_type], TK_EOF
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

;;
;; Read as much data as possible from a file descriptor.
;;
;; r12: fd
;; r13: buffer data
;; r14: buffer size
;; r15: buffer capacity
;;
;; returns: 1 on error, 0 on success.
read:
    push rbx

    ;; Check if we have space in the buffer.
.read_loop:
    cmp r14, r15
    jb .perform_read

    ;; If not, increase the capacity
    add r15, 2048
    shl r15, 1
    mov rdi, r13
    mov rsi, r15
    call realloc
    mov r13, rax

.perform_read:
    mov eax, SYS_read
    mov rdi, r12
    lea rsi, [r13 + r14]
    mov rdx, r15
    sub rdx, r14
    mov rbx, rdx
    syscall

    ;; Check for errors.
    test rax, rax
    jl .read_error

    ;; If we read less than requested, then we’re done.
    add r14, rax
    cmp rax, rbx
    je .read_loop

    xor eax, eax
    jmp .return

.read_error:
    lea rdi, [string_format_read_errno]
    mov esi, eax
    neg esi
    xor eax, eax
    call printf
    mov eax, 1

.return:
    pop rbx
    ret

;;
;; Read a file into memory.
;;
;; rdi: u8* filename
;; rsi: u8** file_contents
;; rdx: u64* file_size
;;
;; returns: 0 on success, 1 on failure.
read_file:
    ;; Scratch regs
    push r12 ; fd
    push r13 ; buffer
    push r14 ; buffer size
    push r15 ; buffer capacity

    ;; Save parameters
    push rdi
    push rsi
    push rdx

    ;; Zero scratch regs.
    xor r12, r12
    xor r13, r13
    xor r14, r14
    xor r15, r15

    ;; Open file.
    xor esi, esi ; O_RDONLY
    mov eax, SYS_open
    syscall
    test rax, rax
    jl .error_open

    ;; Save fd.
    mov r12, rax
    call read
    jnz .error_read

    ;; Save file size and buffer.
    mov rax, [rsp]
    mov [rax], r14
    mov rax, [rsp + 8]
    mov [rax], r13

    ;; Done.
    xor r15, r15 ; Return value.
    jmp .close_file

.error_read:
    lea rsi, [error_read_failed]
    jmp .error_print

.error_open:
    lea rsi, [error_open_read_failed]

.error_print:
    xor eax, eax
    mov rdx, [rsp + 16]
    lea rdi, [string_format_read_file_error]
    call printf
    mov r15, 1 ; Return value.

    ;; Free the buffer.
    mov rdi, r13
    call free

    ;; Close file if open.
.close_file:
    test r12, r12
    jz .return
    mov rdi, r12
    mov eax, SYS_close
    syscall

.return:
    mov rax, r15 ; Return value.
    add rsp, 24
    pop r15
    pop r14
    pop r13
    pop r12
    ret

;;
;; Initialise the lexer.
;;
lexer_init:
    mov byte [lastc], ' '
    mov rdi, [file_contents]
    mov [lexer_curr], rdi
    add rdi, [file_size]
    mov [lexer_end], rdi
    ret

;;
;; Get the next character.
;;
;; clobbers: rdi, rax
;; returns: the next character
next_char:
    ;; Keep returning EOF if we’re at eof.
    mov rdi, [lexer_curr]
    cmp rdi, [lexer_end]
    je .eof

    ;; Read the next char
    inc qword [lexer_curr]
    movzx eax, byte [rdi]
    mov byte [lastc], al
    jmp .return

.eof:
    mov byte [lastc], 0
    xor eax, eax

.return:
    ret

;;
;; Lex the next token.
;;
next_token:
    call lexer_skip_whitespace

    ;; Keep returning EOF if we’re at eof.
    movzx eax, byte [lastc]
    test eax, eax
    jne .init_token

    ;; Return eof.
    mov qword [token_type], TK_EOF
    ret

.init_token:
    ;; Set starting position etc.
    mov rdi, [lexer_curr]
    sub rdi, [file_contents]
    dec rdi
    mov dword [token_location], edi
    mov dword [token_location + 4], 0
    mov qword [token_type], TK_INVALID

;; Single-character tokens: +,;(){}-/*<>
%macro test_single_char 2
    cmp eax, %1
    jne %%next
    mov qword [token_type], %2
    mov dword [token_location + 4], 1
    call next_char
    ret
%%next:
%endmacro

;; Single or double-character tokens: =, ==, !, !=
%macro test_double_char 4
    cmp eax, %1
    jne %%next
    call next_char
    cmp eax, %2
    je %%double
    mov qword [token_type], %3
    mov dword [token_location + 4], 1
    ret
%%double:
    call next_char
    mov qword [token_type], %4
    mov dword [token_location + 4], 2
    ret
%%next:
%endmacro

    test_single_char  0,  TK_EOF
    test_single_char '+', TK_PLUS
    test_single_char ',', TK_COMMA
    test_single_char ';', TK_SEMICOLON
    test_single_char '(', TK_LPAREN
    test_single_char ')', TK_RPAREN
    test_single_char '{', TK_LBRACE
    test_single_char '}', TK_RBRACE
    test_single_char '-', TK_MINUS
    test_single_char '*', TK_STAR
    test_single_char '/', TK_SLASH
    test_single_char '<', TK_LT
    test_single_char '>', TK_GT

    test_double_char '=', '=', TK_ASSIGN, TK_EQ_EQ
    test_double_char '!', '=', TK_NOT, TK_NE

%unmacro test_single_char 2
%unmacro test_double_char 4

    ;; Integer.
    lea rsi, [rax - '0']
    cmp rsi, 9
    jbe .integer

    ;; Identifier.
    cmp eax, '_'
    je .identifier
    lea rsi, [rax - 'a']
    cmp rsi, 'z' - 'a'
    jbe .identifier
    lea rsi, [rax - 'A']
    cmp rsi, 'Z' - 'A'
    jbe .identifier

    ;; Default case. Print an error and skip the character.
    call next_char
    mov dword [token_location + 4], 1
    mov byte [has_error], 1

    push rax
    call print_location
    pop rax

    lea rdi, [string_format_unexpected_character]
    mov rsi, rax
    mov rdx, rax
    xor eax, eax
    jmp printf

.integer:
    xor r8, r8
    xor r9, r9 ; Length.
    mov qword [token_type], TK_INTEGER

.parse_number:
    ;; Multiply by 10 and check for overflow.
    mov rdx, r8
    imul r8, 10
    cmp r8, rdx
    jb .overflow

    ;; Add the digit and check for overflow.
    inc r9
    mov rdx, r8
    add r8, rsi ; Digit value is in rsi.
    cmp r8, rdx
    jb .overflow

    ;; Yeet the character.
    call next_char

    ;; Keep going so long as it’s a digit.
    lea rsi, [rax - '0']
    cmp rsi, 9
    jbe .parse_number

    ;; Save the number.
    mov qword [token_int_value], r8
    mov dword [token_location + 4], r9d
    ret

.overflow:
    mov qword [token_type], TK_INVALID
    sub rsp, 8
    call print_location
    add rsp, 8
    mov rdi, error_integer_overflow
    mov byte [has_error], 1
    jmp puts

.identifier:
    push r12
    xor r12, r12 ; Length.

.parse_identifier:
    inc r12
    call next_char

    cmp eax, '_'
    je .parse_identifier
    lea rsi, [rax - 'a']
    cmp rsi, 'z' - 'a'
    jbe .parse_identifier
    lea rsi, [rax - 'A']
    cmp rsi, 'Z' - 'A'
    jbe .parse_identifier
    lea rsi, [rax - '0']
    cmp rsi, 9
    jbe .parse_identifier

    ;; Save the length.
    mov dword [token_location + 4], r12d

    ;; Compare the identifier with known keywords.
    push r13
    push r14 ; This also aligns the stack.
    lea r13, [table_keywords]
    mov r14d, dword [token_location]
    add r14, [file_contents]

.check_keyword:
    mov rdi, r13 ; Keyword table entry.
    mov rsi, r14 ; Keyword
    mov rdx, r12 ; Length
    call strncmp
    jz .keyword
    add r13, TOKEN_STRING_LENGTH
    cmp r13, table_keywords_end
    jb .check_keyword

    mov qword [token_type], TK_IDENT

.restore_registers:
    pop r14
    pop r13
    pop r12
    ret

.keyword:
    ;; Keyword type is index in table.
    sub r13, table_token_names
    shr r13, TOKEN_STRING_LENGTH_LOG2
    mov qword [token_type], r13
    jmp .restore_registers

;;
;; Skip whitespace.
;;
;; This checks for \t, \n, \v, \f, \r, and ' '.
lexer_skip_whitespace:
    movzx eax, byte [lastc]
    sub eax, 9
    cmp eax, 5
    jb .again
    cmp eax, 23
    je .again
    ret

.again:
    call next_char
    jmp lexer_skip_whitespace

;;
;; Print the current location.
;;
print_location:
    lea rdi, [string_format_location]
    mov rsi, qword [filename]
    mov edx, dword [token_location]
    mov ecx, dword [token_location + 4]
    xor eax, eax
    jmp printf

;;
;; Print the current token.
;;
print_token:
    sub rsp, 8 ; Align the stack.

    ;; Print location.
    call print_location

    ;; Print token type.
    mov rsi, qword [token_type]
    shl rsi, TOKEN_STRING_LENGTH_LOG2
    lea rsi, [table_token_names + rsi]
    lea rdi, [string_format_string]
    xor eax, eax
    call printf

    ;; Print integer value and identifier name, if applicable.
    mov rax, qword [token_type]
    cmp rax, TK_INTEGER
    je .print_integer
    cmp rax, TK_IDENT
    je .print_ident

    mov edi, `\n`
    call putchar
    jmp .return

.print_integer:
    lea rdi, [string_format_integer_token_value]
    mov rsi, qword [token_int_value]
    xor eax, eax
    call printf
    jmp .return

.print_ident:
    lea rdi, [string_format_ident_token_value]
    mov esi, dword [token_location + 4] ; Length first because this is a `%*s` specifier.
    mov edx, dword [token_location]
    add rdx, [file_contents]
    xor eax, eax
    call printf

.return:
    add rsp, 8
    ret
