#args:         rdi, rsi, rdx, rcx, r8, r9
#return:       rax
#caller saved: rax, rdi, rsi, rdx, rcx, r8, r9, r10, and r11;
#callee saved: rbx, rsp, rbp, r12, r13, r14, and r15;
.text
.globl return_S1_asm

return_S1_asm:
    ret

.globl return_S2_asm

return_S2_asm:
    ret

.globl return_S2_c_via_call_asm

return_S2_c_via_call_asm:
    ret
