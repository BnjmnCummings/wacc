.intel_syntax noprefix
.globl main
.section .rodata
.text
main:
	push rbp
	push rbx
	mov rbp, rsp
	mov edi, -1
	# statement primitives do not return results (but will clobber r0/rax)
	call _exit
	mov rax, 0
	pop rbx
	pop rbp
	ret

_exit:
	push rbp
	mov rbp, rsp
	# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
	and rsp, -16
	call exit@plt
	mov rsp, rbp
	pop rbp
	ret
 