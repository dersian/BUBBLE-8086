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

	mov eax, 43512 ; eax holds input integer
	mov ebx, 10	; divider
	xor ecx, ecx ; counter for digits to be printend -> set it to zero

	; Store digits on stack
getNextDigit:
	inc ecx ; increase digit counter by 1
	xor edx, edx
	div ebx ; divide eax by 10
	push dx ; store remainder on stack
	test eax, eax ; check if rest is zero -> [cmp eax, 0]
	jnz getNextDigit ; jump if not zero

	; alle digits op de stack pushen tot dat nul is bereikt(alle cijfers behandeld zijn)

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
