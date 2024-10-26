	.text
main:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	subq	$8, %rsp
	movq	%rsp, -24(%rbp)
	movq	$17, %rax
	movq	-24(%rbp), %rcx
	movq	%rax, (%rcx)
	subq	$8, %rsp
	movq	%rsp, -40(%rbp)
	movq	-24(%rbp), %rax
	movq	-40(%rbp), %rcx
	movq	%rax, (%rcx)
	movq	-40(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, -56(%rbp)
	movq	-56(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, -64(%rbp)
	movq	-64(%rbp), %rax
	movq	%rbp, %rsp
	popq	%rbp
	retq	