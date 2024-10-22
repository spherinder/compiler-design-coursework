	.text
sum:
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
	movq	32(%rbp), -72(%rbp)
	movq	40(%rbp), -80(%rbp)
	.text
ret_last:
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
	movq	32(%rbp), -72(%rbp)
	movq	40(%rbp), -80(%rbp)
	.text
main:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)