
IDEAL
P386
MODEL FLAT, C
ASSUME cs:_TEXT,ds:FLAT,es:FLAT,fs:FLAT,gs:FLAT


CODESEG

start:
     sti            ; set The Interrupt Flag => enable interrupts
     cld            ; clear The Direction Flag

	;value to check
	mov eax, 2

	cmp eax,1
	je is_one

	cmp eax,2
	je is_two

	mov edx, offset msg3
	jmp printme

	is_two:
	mov edx, offset msg2
	jmp printme

	is_one:
	mov edx, offset msg1

	printme:
	mov ah, 09h
	int 21h
	

	; Wait for keystroke and read character.
	mov ah,00h
	int 16h

	; Terminate process with return code in response to a keystroke.
    mov	ax,4C00h
	int 	21h


DATASEG
	msg1	db "eax = 1", 13, 10, '$'
	msg2	db "eax = 2", 13, 10, '$'
	msg3	db "eax is neither", 13, 10, '$'


STACK 100h

END start
