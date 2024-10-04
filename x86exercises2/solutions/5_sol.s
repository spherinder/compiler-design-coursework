#args:         rdi, rsi, rdx, rcx, r8, r9
#return:       rax
#caller saved: rax, rdi, rsi, rdx, rcx, r8, r9, r10, and r11;
#callee saved: rbx, rsp, rbp, r12, r13, r14, and r15;
.text
.globl sum_all_members_S1_asm

sum_all_members_S1_asm:
    movq    %rdi, %rax
    addq    %rsi, %rax
    ret

.globl sum_all_members_S2_asm

sum_all_members_S2_asm:
    movq    8(%rsp), %rax
    addq    16(%rsp), %rax
    addq    24(%rsp), %rax
    ret
