; -------------------------------------------------------------------
; 80386
; 32-bit x86 assembly language
; TASM
;
; author:	David Blinder
; date:		1/10/2010
; program:	calculate Catalan numbers using the stack
; -------------------------------------------------------------------

IDEAL
P386
MODEL FLAT, C
ASSUME cs:_TEXT,ds:FLAT,es:FLAT,fs:FLAT,gs:FLAT

; -------------------------------------------------------------------
; CODE
; -------------------------------------------------------------------
CODESEG

start:
    sti  ; set The Interrupt Flag => enable interrupts
    cld  ; clear The Direction Flag

	xor	edi, edi 	; current Catalan number counter
	push 1 			; push 0th Catalan number C_0 = 1
nextCatalan:
	inc edi		 ; compute next Catalan number #EDI
	xor ebx, ebx ; accumulate next Catalan number in EBX, init to 0
	mov ecx, edi ; decrementing counter (i) iterating over stack
	mov esi, esp ; relative offset from stack top in ESI
accumLoop:
	mov edx, [esp + 4*ecx - 4]	; take Catalan number C_i, go from bottom of stack upwards
	lodsd ; This instruction is equivalent to:
			; mov eax, [esi]	; take Catalan number C_(n-i)
			; add esi, 4		; increase offset by DWORD = 4 BYTES, go from stack top downwards
	
	mul edx			; multiply pair together, get result in EAX
	add ebx, eax	; accumulate product of Catalan numbers in EBX
	loop accumLoop	; next pair of Catalan numbers
	
	push ebx		; push new computed Catalan number on stack
	cmp edi, 10	 	; reached target Catalan number C_10?
	jne nextCatalan ; if not, continue outer loop
	
	mov eax, ebx	; place final result in EAX for printing
	lea esp, [esp + 4*edi + 4] ; restore stack pointer
	; Note, you could also use the less 'fancy':
		; inc edi		; number of Catalan stack elements = EDI + 1 (also include zeroth element C_0)
		; shl edi, 2	; multiply EDI by 4 to get #bytes = 4 * #elements, every element = 32 bits = 4 bytes
		; add esp, edi	; restore stack pointer
	
; (identical to PRINTINT onward)	
printme:
	mov	ebx, 10		; divider
	xor ecx, ecx	; counter for digits to be printed

	; Store digits on stack
getNextDigit:
	inc	ecx         ; increase digit counter
	xor edx, edx
	div	ebx   		; divide by 10
	push dx			; store remainder on stack
	test eax, eax	; check whether zero?
	jnz	getNextDigit

    ; Write all digits to the standard output
	mov	ah, 2h 		; Function for printing single characters.
printDigits:		
	pop dx
	add	dl,'0'      	; Add 30h => code for a digit in the ASCII table, ...
	int	21h            	; Print the digit to the screen, ...
	loop printDigits	; Until digit counter = 0.
	
	mov	dl, 0Dh		; Carriage return.
	int	21h
	mov	dl, 0Ah		; New line.
	int 21h

terminate:
	; Wait for keystroke and read character.
    mov ah,0h
    int 16h

	; Terminate process with return code in response to a keystroke.
	mov	ax,4C00h
	int 21h

; -------------------------------------------------------------------
; DATA
; -------------------------------------------------------------------
DATASEG

; -------------------------------------------------------------------
; STACK
; -------------------------------------------------------------------
STACK 100h

END start
