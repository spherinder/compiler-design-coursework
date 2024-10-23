	.text
factorial:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -8(%rbp)
	jmp	*factorial.start
	.text
factorial.start:
	movq	-24(%rbp), %r8 
	cmpq	$0, %r8 
	jne	*factorial.then
	je	*factorial.end
	.text
factorial.then:
	jmp	*factorial.start
	.text
factorial.end:
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
	movq	-40(%rbp), %rax
	movq	%rbp, %rsp
	popq	%rbp
	retq	