; -------------------------------------------------------------------
; 80386
; 32-bit x86 assembly language
; TASM
;
; author:	Stijn Bettens, David Blinder
; date:		25/09/2017
; program:	branching
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
	; Assign a value to eax.
     mov eax, 1

; 2. Comparison and branching
; ---------------------------
	; Compare eax with 0. If eax == 0, then write msg_zero to the
     ; standard output. otherwise, test for eax == 1
	cmp	eax, 0
	jz is_zero
	
	; If eax == 1, then output msg_one
	cmp eax, 1
	jz is_one
	
	; otherwise, signal that eax is neither 0 nor 1
	mov edx, offset msg_neither
	jmp printme
	
	is_zero:
    mov  edx, offset msg_zero
	jmp	printme

	is_one:
    mov  edx, offset msg_one
	
; 3. Print string
; ---------------------------
	printme:
	mov	ah,09h
    int  21h

; 4. Terminate process
; --------------------
	; Wait for keystroke and read character.
    mov  ah,00h
    int  16h

	; Terminate process with return code in response to a keystroke.
	mov	ax,4C00h
    int  21h

; -------------------------------------------------------------------
; DATA
; -------------------------------------------------------------------
DATASEG
	msg_zero      db "eax = 0", 13, 10, '$'
	msg_one       db "eax = 1", 13, 10, '$'
	msg_neither	  db "eax != 0 and eax != 1", 13, 10, '$'

; -------------------------------------------------------------------
; STACK
; -------------------------------------------------------------------
STACK 100h

END start
