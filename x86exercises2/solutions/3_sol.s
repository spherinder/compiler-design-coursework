#args:         rdi, rsi, rdx, rcx, r8, r9
#return:       rax
#caller saved: rax, rdi, rsi, rdx, rcx, r8, r9, r10, and r11;
#callee saved: rbx, rsp, rbp, r12, r13, r14, and r15;
.text
.globl sum_last_args_asm

sum_last_args_asm:
    # arg7 and arg8 are stored on the stack by the caller
    movq 8(%rsp), %rax  # arg7
    addq 16(%rsp), %rax # arg8
    addq %r9, %rax
    ret
