
IDEAL
P386
MODEL FLAT, C
ASSUME cs:_TEXT,ds:FLAT,es:FLAT,fs:FLAT,gs:FLAT


CODESEG

start:
     sti            ; set The Interrupt Flag => enable interrupts
     cld            ; clear The Direction Flag

	;value to check

	

	mov ah, 09h
	mov edx, offset msg1
	mov ecx, 10

printloop:
	int 21h
	loop printloop ;integreerd de counter ecx automatisch


	; Wait for keystroke and read character.
	mov ah,00h
	int 16h

	; Terminate process with return code in response to a keystroke.
    mov	ax,4C00h
	int 	21h



DATASEG
	msg1	db "hallo ", 13, 10, '$'


STACK 100h

END start
