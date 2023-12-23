BITS 64

segment .text
	global write
	global strlen
	global _start

	extern main

	strlen:
        push  rbx                 ; save any registers that
        push  rcx                 ; we will trash in here

        mov   rbx, rdi            ; rbx = rdi

        xor   al, al              ; the byte that the scan will
                                                            ; compare to is zero

        mov   rcx, 0xffffffff     ; the maximum number of bytes
                                                            ; i'm assuming any string will
                                                            ; have is 4gb

        repne scasb               ; while [rdi] != al, keep scanning

        sub   rdi, rbx            ; length = dist2 - dist1
        mov   rax, rdi            ; rax now holds our length

        pop   rcx                 ; restore the saved registers
        pop   rbx

        ret

	write:
		mov rax, 1
		syscall
        ret

	exit:
		mov rax, 60
		syscall
		ret
	
	_start:
	    push rbp
	    mov rbp, rsp

		call main

	    mov rdi, 0
		call exit

		pop rbp
		ret

