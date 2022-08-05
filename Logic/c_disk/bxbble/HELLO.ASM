IDEAL
P386
MODEL FLAT, C
ASSUME cs:_TEXT,ds:FLAT,es:FLAT,fs:FLAT,gs:FLAT

; -------------------------------------------------------------------
; CODE
; -------------------------------------------------------------------
CODESEG		

PROC fuckupthisshit
	ARG	@@printval:dword
	USES eax, ebx, edx
	xor eax, eax
	xor ebx, ebx
	xor edx, edx
	mov eax, [@@printval]
	mov ebx, 10
	div ebx
	xor edx, edx
	mov [@@printval], eax

ENDP fuckupthisshit
	
PROC printUnsignedInteger
	ARG	@@printval:dword
	USES eax, ebx, ecx, edx

	xor edx, edx

	mov eax, [@@printval]
;	mov eax, [@@modulo]
;	mov bl ,al
;	xor eax, eax
;	mov al, bl
	and eax, 0b111 ;andreas ID


	mov	ebx, 10		; divider
	xor ecx, ecx	; counter for digits to be printed

	; Store digits on stack
@@getNextDigit:
	inc	ecx         ; increase digit counter
	xor edx, edx
	div	ebx   		; divide by 10
	push dx			; store remainder on stack
	test eax, eax	; check whether zero?
	jnz	@@getNextDigit

    ; Write all digits to the standard output
	mov	ah, 2h 		; Function for printing single characters.
@@printDigits:		
	pop dx
	add	dl,'0'      	; Add 30h => code for a digit in the ASCII table, ...
	int	21h            	; Print the digit to the screen, ...
	loop @@printDigits	; Until digit counter = 0.
	
	ret
ENDP printUnsignedInteger
	
PROC main
	sti
	cld

	;call fuckupthisshit, [modulo]
	call printUnsignedInteger, 453698
	

	mov ah,0h		; wait for keystroke
    int 16h
	mov	ax,4C00h 	; terminate
	int 21h
ENDP main

; -------------------------------------------------------------------
; DATA
; -------------------------------------------------------------------
DATASEG

	modulo dd 5779

; -------------------------------------------------------------------
; STACK
; -------------------------------------------------------------------
STACK 100h

END main
