; -------------------------------------------------------------------
; 80386
; 32-bit x86 assembly language
; TASM
;
; author:	David Blinder
; date:		23/10/2017
; program:	Template for exercise session 3
; -------------------------------------------------------------------

IDEAL
P386
MODEL FLAT, C
ASSUME cs:_TEXT,ds:FLAT,es:FLAT,fs:FLAT,gs:FLAT

; -------------------------------------------------------------------
; CODE
; -------------------------------------------------------------------
CODESEG		
	
PROC printUnsignedInteger
	ARG @@printval:dword
	USES eax, ebx, ecx, edx

	mov eax, [@@printval] ; printvalue uitlezen
	mov ebx, 10
	xor ecx, ecx

	; Store digits on stack
@@getNextDigit: ;globale labels
	inc ecx ; increase digit counter
	xor edx, edx
	div ebx ; divide by 10
	push dx
	test eax, eax ; check if zero -> [cmp eax, 0]
	jnz @@getNextDigit

	mov ah, 2h
@@printDigits: 
	pop dx
	add dl, '0' ; add30h => code for a digit in the ASCII table, 0 -> karakter 0
	int 21h ; Print the digit to the screen
	loop @@printDigits ; until digit counter = 0;	

	ret
ENDP printUnsignedInteger
	
PROC main
	sti
	cld

	call printUnsignedInteger, 330
	mov	ah, 2h	; print enter
	mov	dl, 0Dh
	int	21h
	mov	dl, 0Ah
	int 21h

	mov ah,0h		; wait for keystroke
    int 16h
	mov	ax,4C00h 	; terminate
	int 21h
	
ENDP main

; -------------------------------------------------------------------
; DATA
; -------------------------------------------------------------------
DATASEG
	arrlen dd 25
	arrdata dd 814724, 905792, 126987, 913376, 632360, 97541, 278499, 546882, 957507, 964889, 157614, 970593, 957167, 485376, 800281, 141887, 421762, 915736, 792208, 959493, 655741, 35712, 849130, 933994, 678736

; -------------------------------------------------------------------
; STACK
; -------------------------------------------------------------------
STACK 100h

END main
