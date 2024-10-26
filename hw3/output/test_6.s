	.text
main:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	$0, %rax
	movq	%rbp, %rsp
	popq	%rbp
	retq	