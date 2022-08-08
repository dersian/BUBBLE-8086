IDEAL
P386
MODEL FLAT, C
ASSUME cs:_TEXT,ds:FLAT,es:FLAT,fs:FLAT,gs:FLAT

ARR_LEN equ 800
ARR_X equ 40
ARR_Y equ 20

RAND_A = 1103515245
RAND_C = 12345


CODESEG		

PROC rand_init
    USES    eax, ecx, edx

    mov     ah, 02ch        ; Get system time
    int     21h

    mov     ax, dx          ; Use time to generate seed in EAX
    shl     eax, 16
    mov     ax, cx

    mov     ah, 02ah        ; Get system date
    int     21h

    shl     ecx, 16         ; Mangle date into the seed in EAX
    mov     cx, dx
    xor     eax, ecx
    
    mov     [rand_seed], eax

    ret
ENDP rand_init


PROC rand16
    USES    edx

    mov     eax, [rand_seed]
    mov     edx, RAND_A
    mul     edx
    add     eax, RAND_C
    mov     [rand_seed], eax
	shr		eax, 30
    inc eax
	
    ret
ENDP rand16

PROC printUnsignedInteger ;used in printInList, prints 1 integer passsed trough with an array
	ARG	@@printval:dword    ; input argument
	USES eax, ebx, ecx, edx ;test

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

PROC printIntList ;Prints array using arrayptr
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


PROC StartGrid
	ARG	@@arrayptr:dword , @@color1:dword, @@color2:dword
	USES eax, ebx, ecx, edx, esi, edi
	
	xor eax, eax  ;ctr for outer
	mov ebx, [@@arrayptr] ;pointer
	xor edx, edx  ;ctr for inner
	xor esi, esi  ;ctr for alternating 1 0
	xor edi, edi  ;ctr for alternating offset

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
		je @@addColor
				
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

	@@addColor:
		call addColor
		
		mov [ebx], ecx ;update array
		inc esi
		inc edx
		jmp @@inner
	
	@@done:
		ret
ENDP StartGrid

PROC addColor
	uses eax
	
	call    rand16

	mov ecx, eax

	ret
ENDP addColor
	

PROC main
	sti
	cld
	
	call rand_init

	call StartGrid, offset arrlen, [color1], [color2]
	;call update2, offset arrlen, [color2], [color1]
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
	
	rand_seed   dd ?
	arrlen dd ARR_LEN
	;arrdata dd 0, 1, 6, 3, 4

	arrdata dd ARR_LEN dup (?) ;empty array with length 5
	shiftedRow dd 0
	color1 dd 2
	color2 dd 3


; -------------------------------------------------------------------
; STACK
; -------------------------------------------------------------------
STACK 100h

END main
