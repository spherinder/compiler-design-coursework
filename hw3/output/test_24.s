	.text
foo:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	$42, %rax
	movq	%rbp, %rsp
	popq	%rbp
	retq	
	.text
bar:
	pushq	%rbp
	movq	%rsp, %rbp
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
	subq	$8, %rsp
	movq	%rsp, -88(%rbp)
	subq	$8, %rsp
	movq	%rsp, -96(%rbp)
	movq	$0, %rax
	movq	-88(%rbp), %rcx
	movq	%rax, (%rcx)
	movq	$100, %rax
	movq	-96(%rbp), %rcx
	movq	%rax, (%rcx)
	movq	-96(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, -120(%rbp)
	movq	$0, -128(%rbp)
	movq	-120(%rbp), %rax
	movq	$0, %rcx
	cmpq	%rax, %rcx
	setne	-128(%rbp)
	movq	-128(%rbp), %r8 
	cmpq	$0, %r8 
	jne	*main.then
	je	*main.else
	.text
main.then:
	subq	$0, %rsp
	leaq	foo(%rip), %r10
	callq	*%r10
	movq	%rax, -24(%rbp)
	movq	-24(%rbp), %rax
	movq	-88(%rbp), %rcx
	movq	%rax, (%rcx)
	jmp	*main.end
	.text
main.else:
	subq	$0, %rsp
	leaq	bar(%rip), %r10
	callq	*%r10
	movq	%rax, -48(%rbp)
	movq	-48(%rbp), %rax
	movq	-88(%rbp), %rcx
	movq	%rax, (%rcx)
	jmp	*main.end
	.text
main.end:
	movq	-88(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, -72(%rbp)
	movq	-72(%rbp), %rax
	movq	%rbp, %rsp
	popq	%rbp
	retq	