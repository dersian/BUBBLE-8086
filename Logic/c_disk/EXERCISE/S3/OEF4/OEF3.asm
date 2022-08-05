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
	
PROC makebloc
	USES edi, eax, ebx, ecx

	;mov edx, 0			; array counter
	mov eax, 0          ; counter voor 0/256 [0 = 0 , 1 = 256]
	mov ecx, 0          ; outer loop counter
	xor edi, edi
@@outerLoop:

	cmp ecx, 16000        ; zijn we klaar?
	je @@done
	mov ebx, 0          ; ebx = 0, inner counter op 0

@@innerLoop:
	cmp eax, 0
	je @@addzero
	cmp eax, 1
	je @@addtwofivesix
	cmp ebx, 200        ; limiet voor de 2de loop
	je @@innerLoopDone     
	inc ebx
	jmp @@innerLoop

@@innerLoopDone:
	inc ecx
	cmp eax, 0
	je @@eaxwasnul
	mov eax, 1
	jmp @@outerLoop

@@eaxwasnul: 
	mov eax, 1          ; set eax to 1
	jmp @@outerLoop

@@addzero:
	
	mov edi, 0
	jmp @@innerLoop
	;mov [array], 0


@@addtwofivesix:
	mov edi, 256
	jmp @@innerLoop

@@done: 	
	ret

ENDP makebloc
	
PROC main
	sti
	cld

	;sort list and print
	call makebloc

	mov ah,0h		; wait for keystroke
    int 16h
	mov	ax,4C00h 	; terminate
	int 21h
ENDP main

; -------------------------------------------------------------------
; DATA
; -------------------------------------------------------------------
DATASEG




; -------------------------------------------------------------------
; STACK
; -------------------------------------------------------------------
STACK 100h

END main
