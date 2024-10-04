Each exercise consists of a .c source file and a .s assembly that must be completed.

For example, 1.c contains:

```
long add_asm(long a, long b);
long add(long a, long b) { return a + b; }

int main() {
    __builtin_printf("add(1,2)      = %ld\n", add(1, 2));
    __builtin_printf("add_asm(1,2)  = %ld\n", add_asm(1, 2));
    __builtin_printf("add(-2,3)     = %ld\n", add(-2, 3));
    __builtin_printf("add_asm(-2,3) = %ld\n", add_asm(-2, 3));
}
```

and 1.s contains:

```
#args:         rdi, rsi, rdx, rcx, r8, r9
#return:       rax
#caller saved: rax, rdi, rsi, rdx, rcx, r8, r9, r10, and r11;
#callee saved: rbx, rsp, rbp, r12, r13, r14, and r15;
.text
.globl add_asm


add_asm: # %rdi = a %rsi = b, return via %rax
    ret
```

the goal of each exercise is to implement the target function in X86,
e.g., in 1.s `add_asm` should do the same as `add` in 1.c

To compile run and test: `make run1` (`make run2` for 2.c etc).

The solutions folder contains the solutions
