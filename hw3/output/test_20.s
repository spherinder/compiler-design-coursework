	.text
bar:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rax
	movq	-16(%rbp), %rcx
	addq	%rax, %rcx
	movq	%rcx, -24(%rbp)
	movq	-24(%rbp), %rax
	movq	%rbp, %rsp
	popq	%rbp
	retq	
	.text
foo:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rdi
	movq	-8(%rbp), %rsi
	subq	$0, %rsp
	leaq	bar(%rip), %r10
	callq	*%r10
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	%rbp, %rsp
	popq	%rbp
	retq	
	.text
main:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	$17, %rdi
	subq	$0, %rsp
	leaq	foo(%rip), %r10
	callq	*%r10
	movq	%rax, -24(%rbp)
	movq	-24(%rbp), %rax
	movq	%rbp, %rsp
	popq	%rbp
	retq	