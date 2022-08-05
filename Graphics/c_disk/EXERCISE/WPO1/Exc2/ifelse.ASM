; -------------------------------------------------------------------
; 80386
; 32-bit x86 assembly language
; TASM
;
; author:	Stijn Bettens, David Blinder
; date:		25/09/2017
; program:	Excercise 2 - If then else!
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

	; Voeg cijfer aan eax toe.
	mov eax, 1
	
	; Conditional.
    cmp eax, 0
	je lblzero
	
	cmp eax, 1
	je lblone
	
	cmp eax, 2
	je lblneither
	;je jumpt enkel als flag aanstaat
	
	; Print string.
	lblzero:
	mov edx, offset msg_zero
	jmp printme
	
	lblone:
	mov edx, offset msg_one
	jmp printme
	
	lblneither:
	mov edx, offset msg_neither
	jmp printme
	; jump gaat altijd zonder conditie
	
	;Printing
	printme:
	mov ah, 09h
	int 21h
	
	mov ah,00h
	int 16h

    mov	ax,4C00h
	int 21h
	
; -------------------------------------------------------------------
; DATA
; -------------------------------------------------------------------
DATASEG
	msg_zero	db "zero", 13, 10, '$'
	msg_one		db "one", 13, 10, '$'
	msg_neither	db "neither", 13, 10, '$'

; -------------------------------------------------------------------
; STACK
; -------------------------------------------------------------------
STACK 100h

END start
