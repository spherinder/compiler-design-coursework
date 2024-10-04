#args:         rdi, rsi, rdx, rcx, r8, r9
#return:       rax
#caller saved: rax, rdi, rsi, rdx, rcx, r8, r9, r10, and r11;
#callee saved: rbx, rsp, rbp, r12, r13, r14, and r15;
.text
.globl ackermann_asm

ackermann_asm:
    pushq %rbp
    movq %rsp, %rbp
    cmpq $0, %rdi
    jne .M_NOT_ZERO
    # if m == 0, return n + 1
    movq $1, %rax
    addq %rsi, %rax
    jmp .EXIT
.M_NOT_ZERO:
    cmpq $0, %rsi
    jne .N_NOT_ZERO
    # if n == 0, return ackermann(m - 1, 1)
    decq %rdi # m - 1
    movq $1, %rsi # n = 1
    callq ackermann_asm # ackermann(m - 1, 1)
    jmp .EXIT
.N_NOT_ZERO:
    # else return ackermann(m - 1, ackermann(m, n - 1))
    decq %rsi # n - 1
    push %rdi # store m, we will need it later
    callq ackermann_asm # ackermann(m, n - 1)
    movq %rax, %rsi # save ackermann(m, n - 1) in rsi
    popq %rdi # restore m
    decq %rdi # m - 1
    callq ackermann_asm # ackermann(m - 1, ackermann(m, n - 1))
.EXIT:
    movq %rbp, %rsp
    popq %rbp
    ret
