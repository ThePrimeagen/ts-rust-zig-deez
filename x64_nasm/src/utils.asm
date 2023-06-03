;; Check a compile-time condition.
%macro static_assert 2
    %if %1 == 0
        %fatal Static assertion failed: %1. %2
    %endif
%endmacro

%macro static_assert 1
    %if %1 == 0
        %fatal Static assertion failed: %1
    %endif
%endmacro

;; Like std::vector.
struc vector
    .data: resq 1
    .size: resq 1
    .capacity: resq 1
endstruc

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
%endmacro

;; Restore non-volatile registers
%macro rstorregs 0
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbp
    pop rbx
%endmacro

;; For debugging.
%macro ??? 0
    call debug_print_all_regs
    int 3
%endmacro

;; Libc functions.
section .text
extern putchar
extern puts
extern printf
extern strncmp
extern exit
extern abort
extern malloc
extern realloc
extern free

CACHE_LINE_SIZE equ 64

;; =============================================================================
;;  Constant data.
;; =============================================================================
section .rodata
error_usage db "Usage: ./lexer [<filename>]", 0
error_duplicate_filename db "ERROR: Must not specify more than one input file", 10, 0
error_open_read_failed db "ERROR: Could not open file for reading", 0
error_stat_failed db "ERROR: Could not stat file", 0
error_mmap_failed db "ERROR: Could not mmap file", 0
error_read_failed db "ERROR: Could not read from file", 0
error_integer_overflow db "ERROR: Integer overflow", 10, 0
error_arch_prctl_failed db "ERROR: arch_prctl failed to set GS register", 10, 0

string_format_read_file_error db "%s '%s' (errno: %i)", 10, 0
string_format_location db "%s at (%u:%u): ", 0
string_format_unexpected_character db "ERROR: Unexpected character U+%hhx ('%c')", 10, 0
string_format_string db "%s", 0
string_format_integer_token_value db " %llu", 10, 0
string_format_ident_token_value db " %.*s", 10, 0
string_format_read_errno db "ERROR: Read failed with errno %d", 10, 0
string_format_debug_register db "%.4s %016llx (%llu)", 10, 0

string_open_mode_read db "rb", 0
string_dot_exit db ".exit", 10, 0
string_default_filename db "<input>", 0
string_default_prompt db ">> ", 0
DEFAULT_PROMPT_SIZE equ $ - string_default_prompt - 1

debug_register_names:
db "rsp:"
db "rbp:"
db "rax:"
db "rbx:"
db "rcx:"
db "rdx:"
db "rsi:"
db "rdi:"
db "r8: "
db "r9: "
db "r10:"
db "r11:"
db "r12:"
db "r13:"
db "r14:"
db "r15:"

section .text

;;
;; For debugging only.
;;
debug_print_all_regs:
    push rsp ; Save value of rsp + 8.
    push rbp
    push rax
    push rbx

    push rcx
    push rdx
    push rsi
    push rdi

    push r8
    push r9
    push r10
    push r11

    push r12
    push r13
    push r14
    push r15

    ;; Align stack.
    sub rsp, 8

    ;; Fix value of old rsp.
    sub qword [rsp + 128], 8

    ;; Print one register.
    %macro print_reg 1
        mov rdi, string_format_debug_register
        lea rsi, [debug_register_names + %1 * 4]
        mov rdx, [rsp + 128 - %1 * 8]
        mov rcx, rdx
        xor rax, rax
        call printf
    %endmacro

    ;; Print all registers.
    %assign i 0
    %rep 16
        print_reg i
        %assign i i + 1
    %endrep

    ;; Restore stack.
    add rsp, 8

    pop r15
    pop r14
    pop r13
    pop r12

    pop r11
    pop r10
    pop r9
    pop r8

    pop rdi
    pop rsi
    pop rdx
    pop rcx

    pop rbx
    pop rax
    pop rbp
    add rsp, 8 ; Do NOT pop rsp as we’ve changed that value.

    ret