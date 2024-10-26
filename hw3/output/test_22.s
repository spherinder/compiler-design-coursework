	.text
baz:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	%rdx, -24(%rbp)
	movq	%rcx, -32(%rbp)
	movq	%r8 , -40(%rbp)
	movq	%r9 , -48(%rbp)
	movq	16(%rbp), -56(%rbp)
	movq	24(%rbp), -64(%rbp)
	movq	-8(%rbp), %rax
	movq	-16(%rbp), %rcx
	addq	%rax, %rcx
	movq	%rcx, -72(%rbp)
	movq	-72(%rbp), %rax
	movq	-24(%rbp), %rcx
	addq	%rax, %rcx
	movq	%rcx, -80(%rbp)
	movq	-80(%rbp), %rax
	movq	-32(%rbp), %rcx
	addq	%rax, %rcx
	movq	%rcx, -88(%rbp)
	movq	-88(%rbp), %rax
	movq	-40(%rbp), %rcx
	addq	%rax, %rcx
	movq	%rcx, -96(%rbp)
	movq	-96(%rbp), %rax
	movq	-48(%rbp), %rcx
	addq	%rax, %rcx
	movq	%rcx, -104(%rbp)
	movq	-104(%rbp), %rax
	movq	-56(%rbp), %rcx
	addq	%rax, %rcx
	movq	%rcx, -112(%rbp)
	movq	-112(%rbp), %rax
	movq	-64(%rbp), %rcx
	addq	%rax, %rcx
	movq	%rcx, -120(%rbp)
	movq	-120(%rbp), %rax
	movq	%rbp, %rsp
	popq	%rbp
	retq	
	.text
bar:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	%rdx, -24(%rbp)
	movq	%rcx, -32(%rbp)
	movq	%r8 , -40(%rbp)
	movq	%r9 , -48(%rbp)
	movq	16(%rbp), -56(%rbp)
	movq	24(%rbp), -64(%rbp)
	movq	-8(%rbp), %rax
	movq	-16(%rbp), %rcx
	addq	%rax, %rcx
	movq	%rcx, -72(%rbp)
	movq	-72(%rbp), %rax
	movq	-24(%rbp), %rcx
	addq	%rax, %rcx
	movq	%rcx, -80(%rbp)
	movq	-80(%rbp), %rax
	movq	-32(%rbp), %rcx
	addq	%rax, %rcx
	movq	%rcx, -88(%rbp)
	movq	-88(%rbp), %rax
	movq	-40(%rbp), %rcx
	addq	%rax, %rcx
	movq	%rcx, -96(%rbp)
	movq	-72(%rbp), %rdi
	movq	-80(%rbp), %rsi
	movq	-88(%rbp), %rdx
	movq	-96(%rbp), %rcx
	movq	-40(%rbp), %r8 
	movq	-48(%rbp), %r9 
	movq	-56(%rbp), %r10
	movq	%r10, 0(%rsp)
	movq	-64(%rbp), %r10
	movq	%r10, -8(%rsp)
	subq	$16, %rsp
	leaq	baz(%rip), %r10
	callq	*%r10
	movq	%rax, -104(%rbp)
	movq	-96(%rbp), %rax
	movq	-48(%rbp), %rcx
	addq	%rax, %rcx
	movq	%rcx, -112(%rbp)
	movq	-112(%rbp), %rax
	movq	-56(%rbp), %rcx
	addq	%rax, %rcx
	movq	%rcx, -120(%rbp)
	movq	-120(%rbp), %rax
	movq	-64(%rbp), %rcx
	addq	%rax, %rcx
	movq	%rcx, -128(%rbp)
	movq	-128(%rbp), %rax
	movq	-104(%rbp), %rcx
	addq	%rax, %rcx
	movq	%rcx, -136(%rbp)
	movq	-136(%rbp), %rax
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
	movq	-8(%rbp), %rdx
	movq	-8(%rbp), %rcx
	movq	-8(%rbp), %r8 
	movq	-8(%rbp), %r9 
	movq	-8(%rbp), %r10
	movq	%r10, 0(%rsp)
	movq	-8(%rbp), %r10
	movq	%r10, -8(%rsp)
	subq	$16, %rsp
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
	movq	$1, %rdi
	subq	$0, %rsp
	leaq	foo(%rip), %r10
	callq	*%r10
	movq	%rax, -24(%rbp)
	movq	-24(%rbp), %rax
	movq	%rbp, %rsp
	popq	%rbp
	retq	