#args:         rdi, rsi, rdx, rcx, r8, r9
#return:       rax
#caller saved: rax, rdi, rsi, rdx, rcx, r8, r9, r10, and r11;
#callee saved: rbx, rsp, rbp, r12, r13, r14, and r15;
.text
.globl sum_arr_asm


sum_arr_asm: # %rdi = *arr %rsi = n, return via %rax
    movq $0, %rax # sum = 0
    movq $0, %rcx # i = 0
.comp:
    cmpq %rsi, %rcx # compare i and n
    jge .end # if i >= n, jump to end
    addq (%rdi, %rcx, 8), %rax # sum += arr[i]
    incq %rcx # i++
    jmp .comp 
.end:
    ret 
