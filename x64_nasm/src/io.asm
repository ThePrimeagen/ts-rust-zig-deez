%line 1 "io.asm"
;; =============================================================================
;;  File I/O.
;; =============================================================================
section .text

IO_INPUT_PADDING_BYTES equ 8

;; Read as much data as possible from a file descriptor.
;;
;; This function handles both reading input from a file and from
;; a tty. As a result, it is a bit complicated because e.g. a read
;; of size 0 means something very different if we’re reading from
;; a tty vs from a file.
;;
;; edi: 1 or 0, depending on whether we’re reading from a tty.
;;
;; r12: fd
;; r13: buffer data
;; r14: buffer size
;; r15: buffer capacity
;;
;; returns rax: 1 on error, 0 on success.
;; returns rdx: 1 if EOF was reached, 0 otherwise.
read:
    push rbx
    push rbp
    sub rsp, 8

    ;; Save argument.
    mov ebp, edi

    ;; Check if we have space in the buffer.
.read_loop:
    mov rax, r14
    add rax, IO_INPUT_PADDING_BYTES ; Padding at the end.
    cmp rax, r15
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
    sub rdx, IO_INPUT_PADDING_BYTES ; We append EIGHT null terminators.
    sub rdx, r14
    syscall

    ;; Check for errors.
    ;; If we read 0 chars, then we’re done.
    test rax, rax
    jz .read_of_size_0
    jl .read_error

    ;; Increment size of buffer.
    add r14, rax

    ;; If we’re reading from a tty, we need to check some things.
    test ebp, ebp
    jnz .tty_checks

    ;; Otherwise, go again.
    jmp .read_loop

.tty_checks:
    ;; If we’re reading from a tty, and the last char is a newline, then we’re done.
    movzx eax, byte [r13 + r14 - 1]
    cmp eax, 10
    je .return_ok

    ;; Otherwise, a read of 0 bytes indicates EOF.
.tty_read_of_size_0:
    test r14, r14
    jnz .read_loop

    ;; Tell the caller that EOF was signalled and replace it w/ a 0.
    mov rdx, 1
    mov byte [r13 + r14 - 1], 0
    jmp .return_eof

.read_of_size_0:
    ;; If we’re reading from a tty, a read of size 0 indicates that the user
    ;; pressed CTRL+D. We want to ignore CTRL+ if the line is not empty, so
    ;; we jump to the code above to check whether there is data in the buffer.
    ;;
    ;; If we’re reading from a file, then EOF indicates that we should really
    ;; stop reading, so we do that instead.
    test ebp, ebp
    jnz .tty_read_of_size_0

.return_ok:
    ;; Not at EOF (only relevant for ttys).
    xor rdx, rdx

.return_eof:
    static_assert IO_INPUT_PADDING_BYTES == 8, "IO_INPUT_PADDING_BYTES was changed. Update the 'mov qword' below."
    mov qword [r13 + r14], 0 ; Zero-terminate the buffer.
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
    add rsp, 8
    pop rbp
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

    ;; Stat the file to get its size.
    sub rsp, SIZE_OF_STRUCT_STAT
    mov rdi, rax
    mov rsi, rsp
    mov eax, SYS_fstat
    syscall
    test rax, rax
    jnz .error_stat

    ;; Get the size and yeet the stat struct.
    mov r15, [rsp + OFFSET_OF_STRUCT_STAT_ST_SIZE]
    add rsp, SIZE_OF_STRUCT_STAT
    mov rax, [rsp]
    mov [rax], r15

    ;; Add extra bytes of padding.
    add r15, IO_INPUT_PADDING_BYTES

    ;; Allocate a buffer of that size.
    mov rdi, r15
    call malloc
    mov r13, rax

    ;; Read the file.
    xor edi, edi ; Not a tty.
    call read
    test rax, rax
    jnz .error_read

    ;; Save buffer.
    mov rax, [rsp + 8]
    mov [rax], r13

    ;; Done.
    xor r15, r15 ; Return value.
    jmp .close_file

.error_stat:
    lea rsi, [error_stat_failed]
    jmp .error_print

.error_read:
    lea rsi, [error_read_failed]
    jmp .error_print

.error_open:
    lea rsi, [error_open_read_failed]

.error_print:
    mov ecx, eax
    neg ecx
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
