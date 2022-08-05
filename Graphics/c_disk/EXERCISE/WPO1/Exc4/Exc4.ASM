; -------------------------------------------------------------------
; 80386
; 32-bit x86 assembly language
; TASM
;
; author:	Stijn Bettens, David Blinder
; date:		25/09/2017
; program:	Excercise 4!
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

	mov ebx, 8 ; set triangle heigth
	mov ah, 2h ; set register for single symbol printing
	mov esi, 1 ; start counter 

printline:
	dec ebx ; initial space count is (height - 1)
	jz printpyramid ; skip drawing if no spaces left
	mov ecx, ebx ; space counter
	mov dl, ' ' ; space cymbol

printspace:
	int 21h
	loop printspace 

printpyramid: 
	mov ecx, esi ; star counter
	mov dl, '*' ; star symbol

printstar:
	int 21h
	loop printstar
	add esi, 2 ; increment star count by 2 per layer

	mov dl, 0Dh
	int 21h
	mov dl, 0Ah
	int 21h

	test ebx, ebx ; equiv cmp ebx,0
	jg printline

	mov ah,00h
	int 16h

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
