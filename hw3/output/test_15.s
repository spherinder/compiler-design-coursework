	.data
	.globl	gbl
gbl:
	.quad	12
	.text
main:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	leaq	gbl(%rip), %rax
	movq	(%rax), %rcx
	movq	%rcx, -24(%rbp)
	movq	-24(%rbp), %rax
	movq	%rbp, %rsp
	popq	%rbp
	retq	