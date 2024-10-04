#args:         rdi, rsi, rdx, rcx, r8, r9
#return:       rax
#caller saved: rax, rdi, rsi, rdx, rcx, r8, r9, r10, and r11;
#callee saved: rbx, rsp, rbp, r12, r13, r14, and r15;
.text
.globl swap_args_asm


swap_args_asm: # %rdi = a %rsi = b
    movq %rdi, %rax # %rax = a
    movq %rsi, %rdi # %rdi = b
    movq %rax, %rsi # %rsi = a
    callq print
    ret
