; -------------------------------------------------------------------
; 80386
; 32-bit x86 assembly language
; TASM
;
; author:	David Blinder
; date:		23/10/2017
; program:	IDEAL Function calls.
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

PROC maxList
	ARG	@@arrayptr:dword RETURNS eax
	USES ebx, ecx
	
	xor eax, eax	; running maximum
	mov ebx, [@@arrayptr]	; pointer
	mov ecx, [ebx]	; length
	
@@findMax:
	add ebx, 4	; go to next integer
	cmp eax, [ebx]	; is running maximum larger than array value?
	jae @@skipSet
	mov eax, [ebx]	; update running maximum
@@skipSet:
	loop @@findMax	; loop over all integers
	
	ret
ENDP maxList

PROC bubbleSort
	ARG	@@arrayptr
	USES eax, ebx, ecx, edx

@@sortPass:
	mov dl, 1	; whether the array is sorted
	mov ebx, [@@arrayptr]	; pointer
	mov ecx, [ebx]	; length
	dec ecx	; (length-1) swap tests
	
@@swaptest:
	add ebx, 4	; go to next integer
	mov eax, [ebx]
	cmp eax, [ebx+4]	; compare two adjacent values
	jbe @@skipSwap		; if pair is ordered, continue
	xchg eax, [ebx+4]	; exchange second value with eax (containing first)
	mov [ebx], eax		; put eax (containing second) in first position
	xor dl, dl			; list was unsorted
@@skipSwap:
	loop @@swaptest	; loop over all integers
	
	test dl, dl		; if unsorted, new pass is needed
	jz @@sortPass
	
	ret
ENDP bubbleSort
	
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
	mov dl, ' '
	int 21h		; print space
	loop @@printInt	; loop over all integers
	
	mov	dl, 0Dh		; Carriage return.
	int	21h
	mov	dl, 0Ah		; New line.
	int 21h
	
	ret
ENDP
	
PROC main
	sti
	cld

	; Print the unsorted int list
	call printIntList, offset arrlen
	
;	; print list maximum
;	call maxList, offset arrlen
;	call printUnsignedInteger, eax
;	mov	ah, 2h	; print enter
;	mov	dl, 0Dh
;	int	21h
;	mov	dl, 0Ah
;	int 21h
;	
;	; sort list and print
;	call bubbleSort, offset arrlen
;	call printIntList, offset arrlen

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
