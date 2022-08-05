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
	ARG @@arrlength:dword ;pointer naar stack
	USES eax, ebx, ecx, edx

	mov ebx, [@@arrlength] ;brackets staan voor inhoud , store pointer in ebx
	mov ecx, [ebx] ;get length counter in ecx

	mov ah, 2h 
	; Store digits on stack
@@printInt:
	add ebx, 4 ; go to next integer
	call printUnsignedInteger, [dword ptr ebx] ; we willen 32bit element meegeven aan functie
	mov dl, ','
	int 21h
	mov dl, ','
	int 21h
	loop @@printInt ; loop over all integers

	mov	dl, 0Dh
	int	21h
	mov	dl, 0Ah
	int 21h

	ret
ENDP printIntList


PROC maxList
	ARG @@arrayptr: dword RETURNS eax
	USES ebx, ecx
	
	xor eax, eax ; running maximum
	mov ebx, [@@arrayptr]
	mov ecx, [ebx] ; length

@@findMax:
	add ebx, 4 ; go to next integer
	cmp eax, [ebx] ; is running maximum larger than array value?
	jae @@skipSet ; jump above or equal -> zelfde als goter of kleiner maar dan voor unsigned
	mov eax, [ebx]
@@skipSet:
	loop @@findMax ; loop over all integers

	ret
ENDP maxList



PROC bubbleSort
	ARG @@arrayptr:dword 
	USES eax, ebx, ecx

@@sortPass:
	mov dl, 1 ; is array gestorteerd? op 0 zetten als paar niet gestorteerd is
	mov ebx, [@@arrayptr]
	mov ecx, [ebx]
	dec ecx

@@swaptest:
	add ebx, 4
	mov eax, [ebx]
	cmp eax, [ebx + 4]
	jbe @@skipSwap ; if pair is ordered, continue
	xchg eax, [ebx + 4] ; exhange second value with eax
	mov [ebx], eax ; put eax in first position
	xor dl, dl ; list was unsorted

@@skipSwap:
	loop @@swaptest ; loop over all integers

	test dl, dl ; if unstorted, new pass is needed
	jz @@sortPass 

	ret
ENDP bubbleSort
	
PROC main
	sti
	cld
	
	; sort list and print
	call bubbleSort, offset arrlen
	call printIntList, offset arrlen
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
