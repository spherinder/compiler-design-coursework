#args:         rdi, rsi, rdx, rcx, r8, r9
#return:       rax
#caller saved: rax, rdi, rsi, rdx, rcx, r8, r9, r10, and r11;
#callee saved: rbx, rsp, rbp, r12, r13, r14, and r15;
.text
.globl sum_asm


sum_asm: # %rdi = n, return via %rax
    xorq %rax, %rax # sum = 0 equivalent to movq $0, %rax
    xorq %r8, %r8 # i = 0
.comp:
    cmpq %rdi, %r8 # compare i to n
    jge .end # if i >= n, jump to end
    addq %r8, %rax # sum += i
    incq %r8 # i++
    jmp .comp 
.end:
    ret # return sum
