#args:         rdi, rsi, rdx, rcx, r8, r9
#return:       rax
#caller saved: rax, rdi, rsi, rdx, rcx, r8, r9, r10, and r11;
#callee saved: rbx, rsp, rbp, r12, r13, r14, and r15;
.text
.globl func

func:
        cmpq    $0, %rsi
        jle     .L6 # 
        leaq    (%rdi,%rsi,8), %rcx 
        xorq    %rdx, %rdx 
        jmp     .L5
.L9:
        addq    $8, %rdi
        addq    %rax, %rdx
        cmpq    %rcx, %rdi
        je      .L1
.L5:
        movq    (%rdi), %rax 
        cmpq    $0, %rax
        jge     .L9
        addq    $8, %rdi
        subq    %rax, %rdx
        cmpq    %rcx, %rdi 
        jne     .L5 
.L1:
        movq    %rdx, %rax 
        ret     
.L6:
        xorq    %rdx, %rdx
        movq    %rdx, %rax 
        ret 

