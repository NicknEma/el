;; file name: hello.asm
printf proto
includelib msvcrt.lib
includelib legacy_stdio_definitions.lib

.data
    messenge db "hello", 13, 0

.code
main proc
    sub rsp, 40h
    mov rcx, offset messenge
    call printf
    add rsp, 40h
    ret
main endp

end
