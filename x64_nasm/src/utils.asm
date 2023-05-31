SYS_read  equ 0
SYS_write equ 1
SYS_open  equ 2
SYS_close equ 3

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

;; Libc functions.
section .text
extern putchar
extern puts
extern printf
extern strncmp
extern exit
extern abort
extern realloc
extern free