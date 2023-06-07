%line 1 "lexer.asm"
;; =============================================================================
;;  Lexer tables.
;; =============================================================================
section .rodata

MAX_KEYWORD_LENGTH       equ 6 ; Max len of keyword is 6 bytes
TOKEN_STRING_LENGTH      equ 8 ; Max len of token name is 16 bytes

;; LUT for converting token types to strings and parsing keywords.
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

;; Masks for masking out high bytes when comparing strings.
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

;; Table for converting characters to token types.
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

;; =============================================================================
;;  Macros.
;; =============================================================================
;;
;; Get the next character. This is a macro so it
;; can be inlined.
;;
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

;; =============================================================================
;;  Lexer implementation.
;; =============================================================================
section .text

;;
;; Initialise the lexer.
;;
lexer_init:
    mov rbp, [gs:context.file_contents]
    xor r12, r12
    mov r13, rbp
    mov r14, [gs:context.file_size]
    add r14, r13
    mov r15, ' '
    ret

;;
;; Lex the next token.
;;
next_token:
    ;; Init token.
    mov r12, r13
    sub r12, rbp
    sub r12, 1
    jmp .skip_ws

align CACHE_LINE_SIZE
.skip_ws:
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
    add r12, 1
    jmp .skip_ws

.done_skipping_ws:
    ;; Shift start of token in place.
    shl r12, 32

    ;; Keep returning EOF if we’re at eof.
    test r15, r15
    je .eof

    ;; Dispatch the character.
    jmp qword [lexer_char_table + r15 * 8]
    ud2

.eof:
    ;; Return eof.
    mov ebx, TK_EOF
    ret

;;
;; Lex an identifier.
;;
align CACHE_LINE_SIZE
lexer_jump_target_ident:
    endbr64
    mov ebx, TK_IDENT

.again:
    ;; Add the character.
    add r12, 1
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
    add r12, 1
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
align CACHE_LINE_SIZE
lexer_jump_target_punctuation:
    endbr64
    add r12, 1
    movzx rbx, byte [lexer_char_to_token_table + r15]
    next_char
    ret

;;
;; Lex double-character tokens.
;;
lexer_jump_target_punctuation_two_chars:
    endbr64
    add r12, 1
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
    add r12, 1
    mov ebx, TK_EQ_EQ
    ret

.neq:
    next_char
    add r12, 1
    mov ebx, TK_NE
    ret

;;
;; Handle unexpected characters.
;;
lexer_jump_target_invalid:
    endbr64

    mov ebx, TK_INVALID

    push r15
    next_char
    add r12, 1
    mov byte [gs:context.has_error], 1

    call print_location

    lea rdi, [string_format_unexpected_character]
    pop rsi
    mov rdx, rsi
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
    static_assert TOKEN_STRING_LENGTH == 8, "LEA below doesn’t work anymore"
    lea esi, [ebx * 8 + table_token_names]
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

;;
;; Print the current location.
;;
print_location:
    lea rdi, [string_format_location]
    mov rsi, qword [gs:context.filename]
    mov rdx, r12
    shr rdx, 32
    mov rcx, r12
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
    add rdi, 1
    jmp .again

.done:
    clc
    ret

.overflow:
    sub rsp, 8
    call print_location
    mov rdi, error_integer_overflow
    mov byte [gs:context.has_error], 1
    call puts
    add rsp, 8
    stc
    ret
