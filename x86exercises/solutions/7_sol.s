#args:         rdi, rsi, rdx, rcx, r8, r9
#return:       rax
#caller saved: rax, rdi, rsi, rdx, rcx, r8, r9, r10, and r11;
#callee saved: rbx, rsp, rbp, r12, r13, r14, and r15;
.text
.globl func

func:
        cmpq    $0, %rsi
        jle     .L6 # if (%rsi  <= 0) return 0;
        leaq    (%rdi,%rsi,8), %rcx # addr1 = addr0 + 8*rsi 
        xorq    %rdx, %rdx # s = 0
        jmp     .L5
.L9:
        addq    $8, %rdi    # addr0 += 8 (increase by 8 bytes)
        addq    %rax, %rdx  # s += v
        cmpq    %rcx, %rdi
        je      .L1 # if (addr0 == addr1) return s;
.L5:
        movq    (%rdi), %rax # v = *addr0
        cmpq    $0, %rax
        jge     .L9 # if (v >= 0)
        addq    $8, %rdi    # addr0 += 8 (increase by 8 bytes)
        subq    %rax, %rdx  # s -= v
        cmpq    %rcx, %rdi 
        jne     .L5   # if (addr0 != addr1) goto .L5
.L1:
        movq    %rdx, %rax 
        ret     # return s
.L6:
        xorq    %rdx, %rdx
        movq    %rdx, %rax 
        ret     # return 0


#long sum_abs(long *arr, long n){
#    long sum = 0l;
#    for (long i = 0l; i < n; ++i) {
#        if (arr[i] >= 0l)
#            sum += arr[i];
#        else 
#            sum -= arr[i];
#    }
#    return sum;
#}
