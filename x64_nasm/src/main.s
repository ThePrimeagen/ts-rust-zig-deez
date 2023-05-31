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

SYS_read  equ 0
SYS_write equ 1
SYS_open  equ 2
SYS_close equ 3

KW_START     equ 20
TK_FUNCTION  equ KW_START
TK_LET       equ KW_START + 1
TK_RETURN    equ KW_START + 2
TK_TRUE      equ KW_START + 3
TK_FALSE     equ KW_START + 4
TK_IF        equ KW_START + 5
TK_ELSE      equ KW_START + 6

MAX_KEYWORD_LENGTH       equ 6 ; Max len of keyword is 6 bytes
TOKEN_STRING_LENGTH      equ 8 ; Max len of token name is 16 bytes
TOKEN_STRING_LENGTH_LOG2 equ 3 ; log2(TOKEN_STRING_LENGTH)

;; LUT for converting token types to strings.
;; Note: Implicitly zero-terminated by the alignment code.
%macro defstr 1
    db %1
    align TOKEN_STRING_LENGTH,db 0
%endmacro

align TOKEN_STRING_LENGTH,db 0
table_token_names:
    defstr "<error>"
    defstr "<eof>"
    defstr "<ident>"
    defstr "<int>"
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

;; Masks for to mask out high bytes when comparing strings.
string_equal_masks:
dq 0
dq 0xff
dq 0xffff
dq 0xffffff
dq 0xffffffff
dq 0xffffffffff
dq 0xffffffffffff
dq 0xffffffffffffff
dq 0xffffffffffffffff


%unmacro defstr 1

%define I lexer_jump_target_ident
%define N lexer_jump_target_number
%define P lexer_jump_target_punctuation
%define T lexer_jump_target_punctuation_two_chars
%define X lexer_jump_target_invalid

;; Lexer jump table.
lexer_char_table:
dq X, X, X, X, X,   X, X, X, X, X,   X, X, X, X, X,   X, X, X, X, X ; 00-19
dq X, X, X, X, X,   X, X, X, X, X,   X, X, X, T, X,   X, X, X, X, X ; 20-39
dq P, P, P, P, P,   P, X, P, N, N,   N, N, N, N, N,   N, N, N, X, P ; 40-59
dq P, T, P, X, X,   I, I, I, I, I,   I, I, I, I, I,   I, I, I, I, I ; 60-79
dq I, I, I, I, I,   I, I, I, I, I,   I, X, X, X, I,   X, X, I, I, I ; 80-99

dq I, I, I, I, I,   I, I, I, I, I,   I, I, I, I, I,   I, I, I, I, I ; 100-119
dq I, I, I, P, X,   P, X, X, X, X,   X, X, X, X, X,   X, X, X, X, X ; 120-139
dq X, X, X, X, X,   X, X, X, X, X,   X, X, X, X, X,   X, X, X, X, X ; 140-159
dq X, X, X, X, X,   X, X, X, X, X,   X, X, X, X, X,   X, X, X, X, X ; 160-179
dq X, X, X, X, X,   X, X, X, X, X,   X, X, X, X, X,   X, X, X, X, X ; 180-199

dq X, X, X, X, X,   X, X, X, X, X,   X, X, X, X, X,   X, X, X, X, X ; 200-219
dq X, X, X, X, X,   X, X, X, X, X,   X, X, X, X, X,   X, X, X, X, X ; 220-239
dq X, X, X, X, X,   X, X, X, X, X,   X, X, X, X, X,   X, X, X, X, X ; 240-259

%undef I
%undef N
%undef P
%undef T
%undef X

lexer_char_to_token_table:
db 0, 0, 0, 0, 0,   0, 0, 0, 0, 0,   0, 0, 0, 0, 0,   0, 0, 0, 0, 0 ; 00-19
db 0, 0, 0, 0, 0,   0, 0, 0, 0, 0,   0, 0, TK_NOT, 0, 0,   0, 0, 0, 0, 0 ; 20-39
db TK_LPAREN, TK_RPAREN, TK_STAR, TK_PLUS, TK_COMMA,   TK_MINUS, 0, TK_SLASH, 0, 0 ; 40-49
db 0, 0, 0, 0, 0,   0, 0, 0, 0, TK_SEMICOLON                        ; 50-59
db TK_LT, TK_ASSIGN, TK_GT, 0, 0,   0, 0, 0, 0, 0,   0, 0, 0, 0, 0,   0, 0, 0, 0, 0 ; 60-79
db 0, 0, 0, 0, 0,   0, 0, 0, 0, 0,   0, 0, 0, 0, 0,   0, 0, 0, 0, 0 ; 80-99

db 0, 0, 0, 0, 0,   0, 0, 0, 0, 0,   0, 0, 0, 0, 0,   0, 0, 0, 0, 0 ; 100-119
db 0, 0, 0, TK_LBRACE, 0,   TK_RBRACE, 0, 0, 0, 0,   0, 0, 0, 0, 0,   0, 0, 0, 0, 0 ; 120-139
db 0, 0, 0, 0, 0,   0, 0, 0, 0, 0,   0, 0, 0, 0, 0,   0, 0, 0, 0, 0 ; 140-159
db 0, 0, 0, 0, 0,   0, 0, 0, 0, 0,   0, 0, 0, 0, 0,   0, 0, 0, 0, 0 ; 160-179
db 0, 0, 0, 0, 0,   0, 0, 0, 0, 0,   0, 0, 0, 0, 0,   0, 0, 0, 0, 0 ; 180-199

db 0, 0, 0, 0, 0,   0, 0, 0, 0, 0,   0, 0, 0, 0, 0,   0, 0, 0, 0, 0 ; 200-219
db 0, 0, 0, 0, 0,   0, 0, 0, 0, 0,   0, 0, 0, 0, 0,   0, 0, 0, 0, 0 ; 220-239
db 0, 0, 0, 0, 0,   0, 0, 0, 0, 0,   0, 0, 0, 0, 0,   0, 0, 0, 0, 0 ; 240-259

;;
;; Static variables.
;;
section .bss
filename resq 1
file_contents resq 1
file_size resq 1

prompt resq 1
prompt_size resq 1

;; State.
has_error resb 1

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
;; Register usage:
;;
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
;; We try to keep as much state as possible in registers, but
;; sometimes we need to do a mini-‘context switch’, so the
;; macros below are used to facilitate that.

;; Save non-volatile registers
%macro saveregs 0
    push rbx
    push rbp
    push r12
    push r13
    push r14
    push r15
    push gs
    sub rsp, 12 ; Align stack to 16-byte boundary.
%endmacro

;; Restore non-volatile registers
%macro rstorregs 0
    add rsp, 12 ; Undo alignment.
    pop gs
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbp
    pop rbx
%endmacro


;;
;; Get the next character.
;; Note: the input is zero-terminated.
;;
;; clobbers: rax
%macro next_char 0
    lea rax, [r13+1]
    movzx r15, byte [r13]
    cmp r13, r14
    cmovb r13, rax
%endmacro

;;
;; Load the span of the current token into rsi/rdi.
;;
;; returns
;;    rdi: start of token (absolute)
;;    esi: length of token
%macro load_token_span 0
    ;; Get pointer to start of token.
    mov rdi, r12
    shr rdi, 32
    add rdi, rbp

    ;; Get size of token.
    mov rsi, r12
    mov esi, esi
%endmacro


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
    dec rdx ; For null-terminator.
    sub rdx, r14
    mov rbx, rdx
    syscall

    ;; Check for errors.
    test rax, rax
    jl .read_error

    ;; If we read less than requested, then we’re done.
    add r14, rax
    mov byte [r13 + r14], 0 ; Zero-terminate the buffer.
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
    mov rbp, [file_contents]
    xor r12, r12
    mov r13, rbp
    mov r14, [file_size]
    add r14, r13
    mov r15, ' '
    ret

;;
;; Lex the next token.
;;
next_token:
    ;; Skip whitespace.
    ;; This checks for \t, \n, \v, \f, \r, and ' '.
    mov rax, r15
    sub eax, 9
    cmp eax, 5
    jb .again
    cmp eax, 23
    jne .done_skipping_ws

.again:
    next_char
    jmp next_token

.done_skipping_ws:
    ;; Keep returning EOF if we’re at eof.
    test r15, r15
    jne .init_token

    ;; Return eof.
    mov r12, r13
    sub r12, rbp
    dec r12
    shl r12, 32
    mov ebx, TK_EOF
    ret

.init_token:
    ;; Set starting position etc.
    mov r12, r13
    sub r12, rbp
    dec r12
    shl r12, 32
    mov ebx, TK_INVALID

    ;; Dispatch the character.
    jmp qword [lexer_char_table + r15 * 8]

;;
;; Lex an identifier.
;;
lexer_jump_target_ident:
    endbr64
    mov ebx, TK_IDENT

.again:
    ;; Add the character.
    inc r12
    next_char

    ;; Check if char is still part of ident.
    mov rsi, r15
    and esi, ~('a' - 'A') ; Convert lowercase to uppercase for comparison
    sub esi, 'A'
    cmp esi, 'Z' - 'A'
    jbe .again
    mov rsi, r15
    sub esi, '0'
    cmp esi, 9
    jbe .again
    cmp r15, '_'
    je .again

    cmp r12d, MAX_KEYWORD_LENGTH
    jbe lex_maybe_keyword
    ret

;;
;; Compare identifier with keywords.
;;
;; Currently, all keywords are shorter than 8 characters, and we’ve
;; ensured that there are at least 8 bytes of padding after the input,
;; so we can just do a string comparison by loading the token span and
;; keyword into a register, masking off the high bits, and comparing
;; them directly as integers.
;;
lex_maybe_keyword:
    lea rdx, qword [table_keywords]

    ;; Extract the identifier.
    load_token_span

    ;; Mask off unused bytes based on size in esi.
    mov rax, qword [rdi]
    and rax, [string_equal_masks + rsi * 8]

.again:
    ;; Compare with a keyword.
    mov rcx, [rdx]
    cmp rax, [rdx]
    je .match

    ;; Load the next one.
    add rdx, TOKEN_STRING_LENGTH
    cmp rdx, table_keywords_end
    jne .again

    ;; Not a keyword.
    ret

.match:
    ;; Set the token type. The keywords are arranged in the same
    ;; order as their token types.
    sub rdx, table_keywords
    shr rdx, 3
    add rdx, KW_START
    mov ebx, edx
    ret

;;
;; Lex a number.
;;
lexer_jump_target_number:
    endbr64
    mov ebx, TK_INTEGER

.again:
    ;; Add digit.
    inc r12
    next_char

    ;; Check if next char is digit.
    mov rsi, r15
    sub esi, '0'
    cmp esi, 9
    jbe .again
    ret

;;
;; Lex single-character tokens.
;;
lexer_jump_target_punctuation:
    endbr64
    inc r12
    movzx rbx, byte [lexer_char_to_token_table + r15]
    next_char
    ret

;;
;; Lex double-character tokens.
;;
lexer_jump_target_punctuation_two_chars:
    endbr64
    inc r12
    cmp r15, '!'
    je .not

    ;; =, ==
    next_char
    cmp r15, '='
    je .eq_eq

    ;; =
    mov ebx, TK_ASSIGN
    ret

    ;; !, !=
.not:
    next_char
    cmp r15, '='
    je .neq

    ;; !
    mov rbx, TK_NOT
    ret

    ;; ==
.eq_eq:
    next_char
    inc r12
    mov ebx, TK_EQ_EQ
    ret

.neq:
    next_char
    inc r12
    mov ebx, TK_NE
    ret

;;
;; Handle unexpected characters.
;;
lexer_jump_target_invalid:
    endbr64

    push r15
    next_char
    inc r12
    mov byte [has_error], 1

    call print_location

    lea rdi, [string_format_unexpected_character]
    pop rsi
    mov rdx, rsi
    xor eax, eax
    jmp printf

;;
;; Convert an integer token to a number.
;;
;; returns: the integer. The carry flag is set on error
stoi:
    load_token_span
    add rsi, rdi ; Convert size to end of token.

    ;; Conversion loop.
    xor rax, rax
.again:
    cmp rdi, rsi
    je .done
    imul rax, 10
    jc .overflow
    movzx rdx, byte [rdi]
    sub rdx, '0'
    add rax, rdx
    jc .overflow
    inc rdi
    jmp .again

.done:
    clc
    ret

.overflow:
    sub rsp, 8
    call print_location
    mov rdi, error_integer_overflow
    mov byte [has_error], 1
    call puts
    add rsp, 8
    stc
    ret

;;
;; Print the current location.
;;
print_location:
    lea rdi, [string_format_location]
    mov rsi, qword [filename]
    mov rdx, r12
    shr rdx, 32
    mov rcx, r12
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
    mov esi, ebx
    shl esi, TOKEN_STRING_LENGTH_LOG2
    lea rsi, [table_token_names + rsi]
    lea rdi, [string_format_string]
    xor eax, eax
    call printf

    ;; Print integer value and identifier name, if applicable.
    mov rax, rbx
    cmp rax, TK_INTEGER
    je .print_integer
    cmp rax, TK_IDENT
    je .print_ident

.newline:
    mov edi, `\n`
    call putchar
    jmp .return

.print_integer:
    call stoi
    jc .return ; Overflow
    lea rdi, [string_format_integer_token_value]
    mov rsi, rax
    xor eax, eax
    call printf
    jmp .return

.print_ident:
    load_token_span
    mov rdx, rdi
    lea rdi, [string_format_ident_token_value]
    xor eax, eax
    call printf

.return:
    add rsp, 8
    ret
