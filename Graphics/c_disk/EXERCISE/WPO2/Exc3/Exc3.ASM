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

	xor edi, edi ; current catalan number count
	push 1 ; push 0th catalan bumber c_0 = 1
	
nextCatalan:
	inc edi ; compute next catalan number #EDI
	xor ebx, ebx ; accumulate the next Catalan number in ENX, init to 0
	mov ecx, edi ; decrementing counter (i) iterating over stack
	mov esi, esp ; relative offset from stack top in ESI

accumLoop: ; 2 pointers gaan elkaar kruisen t->b en b->t
	mov edx, [esp + 4*ecx - 4] ; take catalan number C_i,go from bottom stack upwards
	lodsd ; this in struction is equivalent to:
			; mov eas, [esi]	; take catalan number C_(n-i)
			; add esi, 4		; increase offset by DWORD = 4 BYTES, go from stack top downwards
	mul edx ; multiply pair togheter, result in EAX
	add ebx, eax ; accu,ulate profuct of Catalan numbers in EBX
	loop accumLoop ; next pair of Catalan numbers

	push ebx ; push new computed Catalan number on stack
	cmp edi, 10 ; reached new target Catalan number c_10?
	jne nextCatalan ; if not, continue outer loop

	



	; Write al digits to the standard output
	mov ah, 2h
printDigits:
	pop dx
	add dl, '0' ; add30h => code for a digit in the ASCII table, 0 -> karakter 0
	int 21h ; Print the digit to the screen
	loop printDigits ; until digit counter = 0;

	mov dl, 0Dh
	int 21h
	mov dl, 0Ah
	int 21h
	
terminate:
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
