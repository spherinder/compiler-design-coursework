#args:         rdi, rsi, rdx, rcx, r8, r9
#return:       rax
#caller saved: rax, rdi, rsi, rdx, rcx, r8, r9, r10, and r11;
#callee saved: rbx, rsp, rbp, r12, r13, r14, and r15;
.text
.globl	reverse_array

reverse_array:
    cmp $0, %rsi
    jle  .EXIT # if n <= 0, return

# Store all array entries on the stack
    mov $0, %rcx
.LOOP:
    cmpq %rsi, %rcx 
    je .EXITLOOP
    push (%rdi, %rcx, 8)
    incq %rcx
    jmp .LOOP

.EXITLOOP:
    # Pop array entries in reverse order
    mov $0, %rcx 
.POPLOOP:
    cmpq %rsi, %rcx 
    je .EXIT
    pop (%rdi, %rcx, 8)
    incq %rcx
    jmp .POPLOOP

.EXIT:
    ret
