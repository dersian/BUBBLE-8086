; -------------------------------------------------------------------
; 80386
; 32-bit x86 assembly language
; TASM
;
; author:	David Blinder
; date:		23/10/2017
; program:	IDEAL Function calls.
;
; Niet in edi schrijven, edi houdt het adress bij waar in het ram de samples naar toe moeten
; Genereer eerst een array in het ram met 0,0,0,0,0,0,0,0,0,255,255,255,255,255,255,255
; Vervang "ReadChunk" met in "Readmusicpiece" met een eigen functie (gebruikmakend van dezelfde argumenten)
; 
; Tip: misschien maar 1/2 blokgolven genereer en dan gewoon someren/manipuleren ipv telkens een nieuwe te maken
;
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
	ARG @@temp_eax:dword, @@temp_ebx:dword, @@temp_ecx:dword, @@arrdata:dword
	USES edi, eax, ebx, ecx

	mov [temp_eax],eax
	mov [temp_ebx],ebx
	mov [temp_ecx],ecx

	;mov edx, 0			; array counter
	mov eax, 0          ; counter voor 0/256 [0 = 0 , 1 = 256]
	mov ecx, 0          ; outer loop counter
	xor edi, edi
@@outerLoop:

	cmp ecx, 2       ; zijn we klaar?
	je @@done
	mov ebx, 0          ; ebx = 0, inner counter op 0
    jmp @@innerLoop

@@innerLoop:	
    inc ebx
	cmp ebx, 8        ; limiet voor de 2de loop
    je @@innerLoopDone 
	cmp eax, 0
	je @@addzero
	cmp eax, 1
	je @@addtwofivesix
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
	mov edi, 255
	jmp @@innerLoop

@@done: 	
	mov eax,[temp_eax]
	mov ebx,[temp_ebx]
	mov ecx,[temp_ecx]
	ret


ENDP makebloc 
	
PROC main
	sti
	cld

	mov eax,1
	mov ebx,2
	mov ecx,3

	call makebloc, [temp_eax], [temp_ebx], [temp_ecx]

	mov ah,0h		; wait for keystroke
    int 16h
	mov	ax,4C00h 	; terminate
	int 21h
ENDP main

; -------------------------------------------------------------------
; DATA
; -------------------------------------------------------------------
DATASEG
	temp_eax dd 0
	temp_ebx dd 0
	temp_ecx dd 0
	arrdata dd 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
; -------------------------------------------------------------------
; STACK
; -------------------------------------------------------------------
STACK 100h

END main
