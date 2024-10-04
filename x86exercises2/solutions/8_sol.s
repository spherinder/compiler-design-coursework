#args:         rdi, rsi, rdx, rcx, r8, r9
#return:       rax
#caller saved: rax, rdi, rsi, rdx, rcx, r8, r9, r10, and r11;
#callee saved: rbx, rsp, rbp, r12, r13, r14, and r15;
.text
.globl	reverse_array2

reverse_array2:
    cmp $0, %rsi
    jle  .EXIT # if n <= 0, return

# allocate storage for n elements
    push %rdi
    push %rsi
    mov %rsi, %rdi
    imulq $8, %rdi # n * sizeof(long)
    callq malloc
    # the allocated array's address is in %rax
    pop %rsi
    pop %rdi
# Store all array entries on the allocated storage
    mov $0, %rcx
.LOOP:
    cmpq %rsi, %rcx 
    je .EXITLOOP
    movq (%rdi, %rcx, 8), %rdx
    movq %rdx, (%rax, %rcx, 8)
    incq %rcx
    jmp .LOOP

.EXITLOOP:
    # Read array entries in reverse order
    mov %rsi, %rcx 
    decq %rcx # rcx = n - 1
    mov $0, %r8 # use a separate counter

.REVLOOP:
    cmpq $0, %rcx 
    jl .EXITREVLOOP
    movq (%rax, %rcx, 8), %rdx # read the n-1, n-2, ... entries from the allocated storage
    movq %rdx, (%rdi, %r8, 8) # write to 0, 1, 2, ... entries of the original array
    decq %rcx 
    incq %r8
    jmp .REVLOOP

.EXITREVLOOP:
    mov %rax, %rdi
    callq free

.EXIT:
    ret
