	.text
main:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	$0, -40(%rbp)
	movq	$3, %rax
	movq	$0, %rcx
	cmpq	%rax, %rcx
	setl	-40(%rbp)
	movq	-40(%rbp), %r8 
	cmpq	$0, %r8 
	jne	*main.then
	je	*main.else
	.text
main.then:
	movq	$7, %rax
	movq	%rbp, %rsp
	popq	%rbp
	retq	
	.text
main.else:
	movq	$9, %rax
	movq	%rbp, %rsp
	popq	%rbp
	retq	