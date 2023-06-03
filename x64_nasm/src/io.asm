;; =============================================================================
;;  File I/O.
;; =============================================================================
section .text

IO_INPUT_PADDING_BYTES equ 8

;; Read as much data as possible from a file descriptor.
;;
;; r12: fd
;; r13: buffer data
;; r14: buffer size
;; r15: buffer capacity
;;
;; returns: 1 on error, 0 on success.
read:
    sub rsp, 8 ; Align stack.

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
    jz .return_ok
    jl .read_error

    ;; Make sure we haven’t changed the padding size
    %if IO_INPUT_PADDING_BYTES != 8
        %error "IO_INPUT_PADDING_BYTES was changed. Update the 'mov qword' below."
    %endif

    ;; Increment size of buffer and go again.
    add r14, rax
    jmp .read_loop

.return_ok:
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
    test rax, rax
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
