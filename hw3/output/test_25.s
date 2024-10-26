	.text
factorial:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -8(%rbp)
	subq	$8, %rsp
	movq	%rsp, -120(%rbp)
	subq	$8, %rsp
	movq	%rsp, -128(%rbp)
	movq	-8(%rbp), %rax
	movq	-120(%rbp), %rcx
	movq	%rax, (%rcx)
	movq	$1, %rax
	movq	-128(%rbp), %rcx
	movq	%rax, (%rcx)
	jmp	*factorial.start
	.text
factorial.start:
	movq	-120(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, -16(%rbp)
	movq	$0, -24(%rbp)
	movq	-16(%rbp), %rax
	movq	$0, %rcx
	cmpq	%rax, %rcx
	setg	-24(%rbp)
	movq	-24(%rbp), %r8 
	cmpq	$0, %r8 
	jne	*factorial.then
	je	*factorial.end
	.text
factorial.then:
	movq	-128(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, -40(%rbp)
	movq	-120(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, -48(%rbp)
	movq	-40(%rbp), %rax
	movq	-48(%rbp), %rcx
	imulq	%rax, %rcx
	movq	%rcx, -56(%rbp)
	movq	-56(%rbp), %rax
	movq	-128(%rbp), %rcx
	movq	%rax, (%rcx)
	movq	-120(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, -72(%rbp)
	movq	-72(%rbp), %rax
	movq	$1, %rcx
	subq	%rax, %rcx
	movq	%rcx, -80(%rbp)
	movq	-80(%rbp), %rax
	movq	-120(%rbp), %rcx
	movq	%rax, (%rcx)
	jmp	*factorial.start
	.text
factorial.end:
	movq	-128(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, -104(%rbp)
	movq	-104(%rbp), %rax
	movq	%rbp, %rsp
	popq	%rbp
	retq	
	.text
main:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	subq	$8, %rsp
	movq	%rsp, -24(%rbp)
	movq	$0, %rax
	movq	-24(%rbp), %rcx
	movq	%rax, (%rcx)
	movq	$5, %rdi
	subq	$0, %rsp
	leaq	factorial(%rip), %r10
	callq	*%r10
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	movq	%rbp, %rsp
	popq	%rbp
	retq	