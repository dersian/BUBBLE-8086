; -------------------------------------------------------------------
; 80386
; 32-bit x86 assembly language
; TASM
;
; author:	Stijn Bettens, David Blinder
; date:		25/09/2017
; program:	Star Pyramid
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
     sti            ; set The Interrupt Flag => enable interrupts
     cld            ; clear The Direction Flag

	mov ebx, 8	; set triangle height
	mov	ah, 2h	; set register for single symbol printing
	mov esi, 1	; star counter

printline:
	dec ebx		; initial space count is (height-1)
	jz printpyramid	; skip drawing if no spaces left
	mov ecx, ebx	; space counter
	mov  dl, ' '	; space symbol
printspace:  
	int	21h
	loop printspace	; equiv. to (dec ecx, jz printspace)

printpyramid:
	mov ecx, esi	; star counter
	mov  dl, '*'	; star symbol
printstar:
	int 21h
	loop printstar
	add esi,2		; increment star count by 2 per layer
	
	mov	dl, 0Dh		; Carriage return.
	int	21h
	mov	dl, 0Ah		; New line.
	int 21h
	
	test ebx, ebx	; equiv. cmp ebx,0
	jg printline

	; Wait for keystroke and read character.
	mov  ah,00h
	int  16h

	; Terminate process with return code in response to a keystroke.
    mov	ax,4C00h
	int 	21h

; -------------------------------------------------------------------
; DATA
; -------------------------------------------------------------------
DATASEG

; -------------------------------------------------------------------
; STACK
; -------------------------------------------------------------------
STACK 100h

END start
