IDEAL
P386
MODEL FLAT, C
ASSUME cs:_TEXT,ds:FLAT,es:FLAT,fs:FLAT,gs:FLAT

ARR_LEN equ 400
ARR_X equ 40
ARR_Y equ 10

CODESEG		
	
PROC printUnsignedInteger
	ARG	@@printval:dword    ; input argument
	USES eax, ebx, ecx, edx

	mov eax, [@@printval]
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

PROC printIntList
	ARG	@@arrayptr:dword
	USES eax, ebx, ecx, edx
	
	mov ebx, [@@arrayptr]	; store pointer in ebx
	mov ecx, [ebx]			; get length counter in ecx
	
	mov	ah, 2h 		; Function for printing single characters.
@@printInt:
	add ebx, 4	; go to next integer
	call printUnsignedInteger, [dword ptr ebx]
	mov dl, ','
	int	21h		; print comma

	loop @@printInt	; loop over all integers
	
	mov	dl, 0Dh		; Carriage return.
	int	21h
	mov	dl, 0Ah		; New line.
	int 21h
	
	ret
ENDP

PROC update2
	ARG	@@arrayptr:dword , @@color1:dword
	USES eax, ebx, ecx, edx, esi, edi

	xor edx, edx  ;ctr for inner
	xor eax, eax  ;ctr for outer
	xor esi, esi  ;ctr for alternating 1 0
	xor edi, edi  ;ctr for alternating offset
	mov ebx, [@@arrayptr]	; pointer
	
	;jmp @@inner
	
	@@outer: 
		xor edx, edx  ;ctr for inner
		inc eax
		inc edi
		cmp eax, ARR_Y + 1
		je @@done
		

		cmp edi, 2  ;time for offset
		je @@blockoffset

		jmp @@noOffset
	@@inner:	   
		cmp edx, ARR_X 
		je @@outer
		
		add ebx, 4
		cmp esi, 0
		je @@addOne
		
		
		xor esi, esi
		inc edx
		jmp @@inner
	
	@@noOffset:
		mov esi, 0
		jmp @@inner
	
	@@blockoffset:
		mov esi, 1
		xor edi, edi
		jmp @@inner

	@@addOne:
		mov ecx, [dword ptr @@color1]
		mov [ebx], ecx
		inc esi
		inc edx
		jmp @@inner

	@@done:
		ret
ENDP update2
	
PROC main
	sti
	cld

	call update2, offset arrlen, [color]
	;call bubbleSort, offset arrlen
	call printIntList, offset arrlen


	mov ah,0h		; wait for keystroke
    int 16h
	mov	ax,4C00h 	; terminate
	int 21h
ENDP main

; -------------------------------------------------------------------
; DATA
; -------------------------------------------------------------------
DATASEG
	
	
	arrlen dd ARR_LEN
	;arrdata dd 0, 1, 6, 3, 4

	arrdata dd ARR_LEN dup (?) ;empty array with length 5
	color dd 6


; -------------------------------------------------------------------
; STACK
; -------------------------------------------------------------------
STACK 100h

END main
