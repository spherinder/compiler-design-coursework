	.text
main:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, %r8 
	movq	%r8 , -8(%rbp)
	movq	%rsi, %r8 
	movq	%r8 , -16(%rbp)
	movq	$10, %rax
	movq	$9, %rcx
	subq	%rax, %rcx
	movq	%rcx, -24(%rbp)
	movq	-24(%rbp), %rax
	movq	%rbp, %rsp
	popq	%rbp
	retq	