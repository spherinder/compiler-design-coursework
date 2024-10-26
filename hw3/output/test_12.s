	.text
f1:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -8(%rbp)
	jmp	*f1.start
	.text
f1.start:
	movq	$0, -16(%rbp)
	movq	-8(%rbp), %rax
	movq	$10, %rcx
	cmpq	%rax, %rcx
	setg	-16(%rbp)
	movq	-16(%rbp), %r8 
	cmpq	$0, %r8 
	jne	*f1.then
	je	*f1.end
	.text
f1.then:
	movq	$1, %rax
	movq	%rbp, %rsp
	popq	%rbp
	retq	
	.text
f1.end:
	movq	$0, %rax
	movq	%rbp, %rsp
	popq	%rbp
	retq	
	.text
f2:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -8(%rbp)
	jmp	*f2.start
	.text
f2.start:
	movq	$0, -16(%rbp)
	movq	-8(%rbp), %rax
	movq	$10, %rcx
	cmpq	%rax, %rcx
	setg	-16(%rbp)
	movq	-16(%rbp), %r8 
	cmpq	$0, %r8 
	jne	*f2.then
	je	*f2.end
	.text
f2.then:
	movq	$1, %rax
	movq	%rbp, %rsp
	popq	%rbp
	retq	
	.text
f2.end:
	movq	$0, %rax
	movq	%rbp, %rsp
	popq	%rbp
	retq	
	.text
main:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	$0, %rdi
	subq	$0, %rsp
	leaq	f1(%rip), %r10
	callq	*%r10
	movq	%rax, -24(%rbp)
	movq	$15, %rdi
	subq	$0, %rsp
	leaq	f2(%rip), %r10
	callq	*%r10
	movq	%rax, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	-32(%rbp), %rcx
	addq	%rax, %rcx
	movq	%rcx, -40(%rbp)
	movq	-40(%rbp), %rax
	movq	%rbp, %rsp
	popq	%rbp
	retq	