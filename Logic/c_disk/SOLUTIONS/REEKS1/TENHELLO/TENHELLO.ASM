; -------------------------------------------------------------------
; 80386
; 32-bit x86 assembly language
; TASM
;
; author:	Stijn Bettens, David Blinder
; date:		25/09/2017
; program:	print 10 times "Hello World!"
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

; 1. Initialization
; -----------------
	mov	ah, 09h
    mov edx, offset msg
	mov	ecx, 10 ; Specify how many times 'Hello World!' should be printed.
 
; 2. Print to the standard output
; -------------------------------
; Print "Hello World!" ecx times
printloop:
	int	21h
	loop printloop	; dec ecx; jnz printloop

; 3. Terminate process
; --------------------
	; Wait for keystroke and read character.
    mov  ah, 00h
    int  16h

	; Terminate process with return code in response to a keystroke.
	mov	ax, 4C00h
	int 21h

; -------------------------------------------------------------------
; DATA
; -------------------------------------------------------------------
DATASEG
	msg		db "Hello World!", 13, 10, '$'

; -------------------------------------------------------------------
; STACK
; -------------------------------------------------------------------
STACK 100h

END start
