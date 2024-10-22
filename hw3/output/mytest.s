	.text
square:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -8(%rbp)
	movq	%rbp, %rsp
	popq	%rbp
	retq	