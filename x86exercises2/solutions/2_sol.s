#args:         rdi, rsi, rdx, rcx, r8, r9
#return:       rax
#caller saved: rax, rdi, rsi, rdx, rcx, r8, r9, r10, and r11;
#callee saved: rbx, rsp, rbp, r12, r13, r14, and r15;
.text
.globl swap_args_asm


swap_args_asm: # %rdi = a %rsi = b
    pushq %rbp
    movq %rsp, %rbp
    subq $16, %rsp
    movq %rdi, (%rsp)  # save a on the stack
    movq %rsi, 8(%rsp) # save b on the stack
    # instead of the previous three instructions, we could have used:
    # pushq %rsi
    # pushq %rdi
    leaq (%rsp), %rsi  # load the address of a's stack slot into %rsi
    leaq 8(%rsp), %rdi # load the address of b's stack slot into %rdi
    # or:
    # leaq -16(%rbp), %rsi  
    # leaq -8(%rbp), %rdi  
    callq print
    # addq $16, %rsp # we don't really need this because we're returning
    movq %rbp, %rsp
    popq %rbp
    ret
