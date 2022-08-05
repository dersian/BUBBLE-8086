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

PROC printIntList
	ARG @@arrayptr:dword
	USES eax, ebx, ecx, edx

	mov ebx, [@@arrayptr]
	mov ecx, [ebx]

	mov ah, 2h

@@printInt:
	add ebx, 4;
	call printUnsignedInteger, [dword ptr ebx]
	mov dl, ','
	int 21h
	mov dl, ' '
	int 21h
	loop @@printInt ;loops till ecx is 0

	mov dl, 0Dh
	int 21h
	mov dl, 0Ah
	int 21h 

	ret


ENDP printIntList

PROC maxList
	ARG @@arrayptr:dword RETURNS eax
	USES ebx, ecx

	xor eax, eax ;eax is ons lopend maximum dus begint op 0
	mov ebx, [@@arrayptr] ; pointer
	mov ecx, [ebx] ;lenght of loop counter ecx

@@findMax:
	add ebx, 4 ;increment with 4 to go to next integer
	cmp eax,  [ebx] ;is running max larger than array value? jae[JumpIfConditionIsMEt]
	jae @@skipSet ; 		> yes? then loop and don't update run max
	mov eax, [ebx] ;update to new runnting max
@@skipSet:
	loop @@findMax

	ret

ENDP maxList

; PROC bubbleSort
; ...
; ENDP bubbleSort
	
PROC main
	sti
	cld

	; Print the unsorted int list
	;call printIntList, offset arrlen
	
	;  print list maximum
	call maxList, offset arrlen
	call printUnsignedInteger, eax
	mov	ah, 2h	; print enter
	mov	dl, 0Dh
	int	21h
	mov	dl, 0Ah
	int 21h
	
	; ; sort list and print
	; call bubbleSort, offset arrlen
	; call printIntList, offset arrlen

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
