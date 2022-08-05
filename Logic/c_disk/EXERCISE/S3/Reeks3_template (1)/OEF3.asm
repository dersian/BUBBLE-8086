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
...
ENDP printUnsignedInteger

; PROC printIntList
; ...
; ENDP printIntList

; PROC maxList
; ...
; ENDP maxList

; PROC bubbleSort
; ...
; ENDP bubbleSort
	
PROC main
	sti
	cld

	; ; Print the unsorted int list
	; call printIntList, offset arrlen
	
	; ; print list maximum
	; call maxList, offset arrlen
	; call printUnsignedInteger, eax
	; mov	ah, 2h	; print enter
	; mov	dl, 0Dh
	; int	21h
	; mov	dl, 0Ah
	; int 21h
	
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
