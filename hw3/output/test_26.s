	.text
factorial:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -8(%rbp)
	movq	$0, -56(%rbp)
	movq	-8(%rbp), %rax
	movq	$0, %rcx
	cmpq	%rax, %rcx
	sete	-56(%rbp)
	movq	-56(%rbp), %r8 
	cmpq	$0, %r8 
	jne	*factorial.ret1
	je	*factorial.recurse
	.text
factorial.ret1:
	movq	$1, %rax
	movq	%rbp, %rsp
	popq	%rbp
	retq	
	.text
factorial.recurse:
	movq	-8(%rbp), %rax
	movq	$1, %rcx
	subq	%rax, %rcx
	movq	%rcx, -24(%rbp)
	movq	-24(%rbp), %rdi
	subq	$0, %rsp
	leaq	factorial(%rip), %r10
	callq	*%r10
	movq	%rax, -32(%rbp)
	movq	-8(%rbp), %rax
	movq	-32(%rbp), %rcx
	imulq	%rax, %rcx
	movq	%rcx, -40(%rbp)
	movq	-40(%rbp), %rax
	movq	%rbp, %rsp
	popq	%rbp
	retq	
	.text
main:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	$5, %rdi
	subq	$0, %rsp
	leaq	factorial(%rip), %r10
	callq	*%r10
	movq	%rax, -24(%rbp)
	movq	-24(%rbp), %rax
	movq	%rbp, %rsp
	popq	%rbp
	retq	