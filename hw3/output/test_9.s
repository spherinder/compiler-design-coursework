	.text
main:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	$5, %rax
	movq	$12, %rcx
	addq	%rax, %rcx
	movq	%rcx, -40(%rbp)
	jmp	*main.next
	.text
main.next:
	jmp	*main.end
	.text
main.end:
	movq	-40(%rbp), %rax
	movq	%rbp, %rsp
	popq	%rbp
	retq	