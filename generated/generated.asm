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
mov r8, 2
mov r9, 3
imul r8, r9
mov r9, 10
mov r10, 4
mov r11, 1
add r10, r11
add r8, r9
mov rax, r8
pop r15
pop r14
pop r13
pop r12
pop rbp
pop rbx
ret
main endp
end
