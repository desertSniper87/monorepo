	.file	"examp__p53__2.c"
	.section	.rodata.str1.1,"aMS",@progbits,1
.LC0:
	.string	"%d"
.LC1:
	.string	"The number is prime"
.LC2:
	.string	"The number is not prime"
	.section	.text.startup,"ax",@progbits
	.p2align 4,,15
	.globl	main
	.type	main, @function
main:
.LFB24:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	xorl	%eax, %eax
	movl	$.LC0, %edi
	leaq	12(%rsp), %rsi
	call	__isoc99_scanf
	movl	12(%rsp), %esi
	movl	%esi, %r9d
	shrl	$31, %r9d
	addl	%esi, %r9d
	sarl	%r9d
	cmpl	$1, %r9d
	jle	.L2
	addl	$1, %r9d
	movl	$1, %edi
	movl	$2, %ecx
	xorl	%r8d, %r8d
	.p2align 4,,10
	.p2align 3
.L5:
	movl	%esi, %eax
	cltd
	idivl	%ecx
	testl	%edx, %edx
	cmove	%r8d, %edi
	addl	$1, %ecx
	cmpl	%r9d, %ecx
	jne	.L5
	cmpl	$1, %edi
	je	.L2
	movl	$.LC2, %esi
	movl	$1, %edi
	xorl	%eax, %eax
	call	__printf_chk
.L7:
	xorl	%eax, %eax
	call	getch
	xorl	%eax, %eax
	addq	$24, %rsp
	.cfi_remember_state
	.cfi_def_cfa_offset 8
	ret
.L2:
	.cfi_restore_state
	movl	$.LC1, %esi
	movl	$1, %edi
	xorl	%eax, %eax
	call	__printf_chk
	jmp	.L7
	.cfi_endproc
.LFE24:
	.size	main, .-main
	.ident	"GCC: (Ubuntu 4.8.4-2ubuntu1~14.04.3) 4.8.4"
	.section	.note.GNU-stack,"",@progbits
