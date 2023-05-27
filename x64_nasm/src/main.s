bits 64
default rel

;;
;; Constant data.
;;
section .rodata
error_usage db "Usage: ./lexer <filename>", 0
error_no_file db "ERROR: An input file is required", 10, 0
error_duplicate_filename db "ERROR: Must not specify more than one input file", 10, 0
error_open_read_failed db "ERROR: Could not open file for reading", 0
error_read_failed db "ERROR: Could not read from file", 0
error_integer_overflow db "ERROR: Integer overflow", 10, 0

string_format_read_file_error db "%s: '%s'", 10, 0
string_format_location db "%s at (%u:%u): ", 0
string_format_unexpected_character db "ERROR: Unexpected character \x%x ('%c')", 10, 0

string_open_mode_read db "rb", 0

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
TK_FUNCTION  equ 12
TK_LET       equ 13

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
table_keywords:
    defstr "fn"
    defstr "let"
table_keywords_end:

%unmacro defstr 1

;;
;; Static variables.
;;
section .bss
filename resq 1
file_contents resq 1
file_size resq 1

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
extern puts
extern printf
extern strncmp
extern fopen
extern fclose
extern fread
extern fwrite
extern exit
extern realloc
extern free
extern stdout ; Works with glibc, not sure about other implementations.

;;
;; Entry point.
;;
global main
main:
    sub rsp, 8 ; Align stack
    call parse_args
    cmp rax, 0
    jnz .error_usage

    mov rdi, [filename]
    cmp rdi, 0
    jz .no_file

    call compiler_main
    mov rdi, rax
    jmp exit

.no_file:
    lea rdi, [error_no_file]
    call puts

.error_usage: 
    lea rdi, [error_usage]

.print_exit:
    call puts
    mov edi, 1
    jmp exit

;;
;; Compiler main function.
;;
;; Returns 0 on success, 1 on failure.
compiler_main:
    sub rsp, 8 ; Align stack

    mov rdi, [filename]
    lea rsi, [file_contents]
    lea rdx, [file_size]
    call read_file
    cmp rax, 0
    jnz .error

    ;; Prime the lexer.
    call lexer_init
    call next_char

.read_next_token:
    call next_token
    call print_token
    cmp qword [token_type], TK_EOF
    jne .read_next_token

    movsx eax, byte [has_error]
    jmp .return

.error:
    mov eax, 1

.return:
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

.parse_one_arg:
    mov rdi, [rsi]
    cmp rdi, 0
    jz .last_arg

    mov rdx, [filename]
    cmp rdx, 0
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
;; Read a file into memory.
;;
;; rdi: u8* filename
;; rsi: u8** file_contents
;; rdx: u64* file_size
;;
;; returns: 0 on success, 1 on failure
read_file:
    ;; Scratch regs
    push r12 ; FILE*
    push r13 ; bytes read
    push r14 ; buffer
    push r15 ; buffer size

    ;; Save parameters
    push rdi
    push rsi
    push rdx

    ;; Zero scratch regs.
    xor r12d, r12d
    xor r13d, r13d
    xor r14d, r14d
    xor r15d, r15d

    ;; Open file.
    mov rsi, string_open_mode_read
    call fopen
    cmp rax, 0
    jz .error_open

    ;; Save FILE* and initialise vars.
    mov r12, rax

.read_chunk:
    ;; Grow buffer.
    mov rdi, r14
    add r15, 4096
    mov rsi, r15
    call realloc
    mov r14, rax

    ;; Read data.
    mov rdi, r14
    add rdi, r13 ; Append to end of buffer.
    mov esi, 1
    mov rdx, r15
    sub rdx, r13 ; Read `buffer size - bytes read` bytes.
    mov rcx, r12
    call fread

    ;; Increment bytes read.
    add r13, rax

    ;; Check for errors and EOF.
    cmp rax, 0
    jl .error_read
    jnz .read_chunk

    ;; Save file size and buffer.
    mov rax, [rsp]
    mov [rax], r13
    mov rax, [rsp + 8]
    mov [rax], r14

    ;; Done.
    xor eax, eax
    jmp .return

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

    ;; Close file if open.
    cmp r12, 0
    jz .file_closed
    mov rdi, r12
    call fclose

.file_closed:
    ;; Free buffer.
    mov rdi, r14
    call free

    ;; Return 1.
    mov eax, 1

.return:
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
    xor rax, rax

.return:
    ret

;;
;; Lex the next token.
;;
next_token:
    call lexer_skip_whitespace

    ;; Keep returning EOF if we’re at eof.
    movzx eax, byte [lastc]
    cmp eax, 0
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

    ;; Single-character tokens: =+,;(){}
%macro test_single_char 2
    cmp eax, %1
    jne %%next
    mov qword [token_type], %2
    mov dword [token_location + 4], 1
    call next_char
    ret
%%next:
%endmacro

    test_single_char  0,  TK_EOF
    test_single_char '=', TK_ASSIGN
    test_single_char '+', TK_PLUS
    test_single_char ',', TK_COMMA
    test_single_char ';', TK_SEMICOLON
    test_single_char '(', TK_LPAREN
    test_single_char ')', TK_RPAREN
    test_single_char '{', TK_LBRACE
    test_single_char '}', TK_RBRACE

%unmacro test_single_char 2

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
    call print_location
    add rsp, 8
    mov rdi, qword [token_type]
    shl rdi, TOKEN_STRING_LENGTH_LOG2
    lea rdi, [table_token_names + rdi]
    jmp puts