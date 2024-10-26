	.text
foo:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	subq	$8, %rsp
	movq	%rsp, -24(%rbp)
	movq	-8(%rbp), %rax
	movq	-16(%rbp), %rcx
	addq	%rax, %rcx
	movq	%rcx, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	-24(%rbp), %rcx
	movq	%rax, (%rcx)
	movq	-24(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, -48(%rbp)
	movq	-48(%rbp), %rax
	movq	%rbp, %rsp
	popq	%rbp
	retq	
	.text
main:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	leaq	foo(%rip), %rdi
	subq	$0, %rsp
	leaq	ll_callback(%rip), %r10
	callq	*%r10
	movq	%rax, -24(%rbp)
	movq	-24(%rbp), %rdi
	subq	$0, %rsp
	leaq	ll_ltoa(%rip), %r10
	callq	*%r10
	movq	%rax, -32(%rbp)
	movq	-32(%rbp), %rdi
	subq	$0, %rsp
	leaq	ll_puts(%rip), %r10
	callq	*%r10
	movq	%rax, -40(%rbp)
	movq	$0, %rax
	movq	%rbp, %rsp
	popq	%rbp
	retq	