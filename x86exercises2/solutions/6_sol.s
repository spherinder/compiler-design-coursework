#args:         rdi, rsi, rdx, rcx, r8, r9
#return:       rax
#caller saved: rax, rdi, rsi, rdx, rcx, r8, r9, r10, and r11;
#callee saved: rbx, rsp, rbp, r12, r13, r14, and r15;
.text
.globl return_S1_asm

return_S1_asm:
    movq $1, %rax
    movq $2, %rdx
    # we return structs of up to 16 bytes via rax and rdx
    ret

.globl return_S2_asm

return_S2_asm:
    movq $3, (%rdi)
    movq $4, 8(%rdi)
    movq $5, 16(%rdi)
    # we return larger structs via an implicit pointer, i.e,
    # struct S2 return_S2_asm(struct S2 *ret);
    ret

.globl return_S2_c_via_call_asm

return_S2_c_via_call_asm:
    push %rbp
    movq %rsp, %rbp
    # we "allocate" space on our stack for an S2 struct
    subq $24, %rsp

    # we pass the address of our stack allocated S2 struct to the caller
    movq %rsp, %rdi
    call return_S2
    # we can read the return value from the stack
    movq 16(%rsp), %rax
    movq %rbp, %rsp
    pop %rbp
    ret
