; Generated
includelib msvcrt.lib
.data
.code
main proc
	push rbx
	push rbp
	push r12
	push r13
	push r14
	push r15
	mov rax, 2
	mov rbx, 3
	imul rax, rbx
	mov rbx, 10
	mov rcx, 4
	mov rdx, 1
	add rcx, rdx
	; Unimplemented instruction '4'
	add rax, rbx
	mov rbx, 7
	mov rcx, 0
	sub rbx, rcx
	; Unimplemented returning of multiple values
	pop r15
	pop r14
	pop r13
	pop r12
	pop rbp
	pop rbx
	ret
main endp
end
