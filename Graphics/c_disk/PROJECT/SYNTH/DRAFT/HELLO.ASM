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

ARR_LENGTH equ 400    ; totale lengte van de blokgolf
ARR_PERIOD equ 200    ; 1/2 van de lengte van 1 blokgolf
ARR_ITTERATIONS equ 2 ; 2 = ARR_DEVIDER


PROC makebloc
	ARG @@temp_eax:dword, @@temp_ebx:dword, @@temp_ecx:dword, @@temp_esi:dword, @@arrdata:dword
	USES eax, ebx, ecx, esi

	mov [temp_eax],eax
	mov [temp_ebx],ebx
	mov [temp_ecx],ecx
	mov [temp_esi],esi

	xor esi, esi
	mov esi,[@@arrdata]

	mov eax, 0          ; counter voor 0/256 [0 = 0 , 1 = 256]
	mov ecx, 0          ; outer loop counter
@@outerLoop:

	cmp ecx, ARR_ITTERATIONS        ;outer, zijn we klaar?
	je @@done
	mov ebx, 0          ; inner counter op 0
    jmp @@innerLoop

@@innerLoop:	
    
	cmp ebx, ARR_PERIOD         ; limiet voor de 2de loop
    je @@innerLoopDone 
	cmp eax, 0
	je @@addzero
	cmp eax, 1
	je @@addtwofivesix
	inc ebx
	jmp @@innerLoop

@@innerLoopDone:
	inc ecx
	cmp eax, 0
	je @@eaxwasnul
	mov eax, 0
	jmp @@outerLoop

@@eaxwasnul: 
	mov eax, 1           ; set eax to 1
	jmp @@outerLoop

@@addzero:
	mov [dword ptr (esi)], 15 ; HEX: F0
	add esi, 4
	inc ebx
	jmp @@innerLoop


@@addtwofivesix:
	mov [dword ptr (esi)], 240 ; HEX: F0
	add esi, 4
	inc ebx
	jmp @@innerLoop

@@done: 	
	mov eax,[temp_eax]
	mov ebx,[temp_ebx]
	mov ecx,[temp_ecx]
	mov esi,[temp_esi]

	ret

ENDP makebloc 

PROC sam
	ARG @@temp_eax:dword, @@temp_ebx:dword, @@temp_ecx:dword, @@temp_esi:dword, @@arrdata:dword
	USES eax, ebx, ecx,esi
	mov [temp_eax],eax
	mov [temp_ebx],ebx
	mov [temp_ecx],ecx
	mov [temp_esi],esi
	
	mov ebx, ARR_LENGTH
	mov ebx, ARR_PERIOD
	mov ebx, ARR_ITTERATIONS

	xor esi, esi
	xor ebx, ebx
	xor eax, eax
	xor ecx, ecx

	mov ebx,[@@arrdata]
@@outerLoop:
	cmp ecx, ARR_LENGTH    		
	je @@done
	mov esi, [dword ptr (ebx+ecx*4)]
	inc ecx
	jmp @@outerLoop
	
@@done:
	mov eax,[temp_eax]
	mov ebx,[temp_ebx]
	mov ecx,[temp_ecx]
	mov esi,[temp_esi]

	ret

ENDP sam
	
PROC main
	sti
	cld

	mov eax,1
	mov ebx,2
	mov ecx,3

	call makebloc, [temp_eax], [temp_ebx], [temp_ecx], [temp_esi], offset arrdata
	call sam, [temp_eax], [temp_ebx], [temp_ecx], [temp_esi], offset arrdata
	;Next step: Either 1 or 2 
	;1 Make the longest array possible right now and get it to play trough the chunk function of playwav/make my own
	;2 Dont make arrays and generate a 16000 array > then do 1

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
	temp_esi dd 0
	;arrdata dd 3,2,3,4,5,6,7,10,0,0,0,0,0,1,0,0
	arrdata dd ARR_LENGTH dup (?)
; -------------------------------------------------------------------
; STACK
; -------------------------------------------------------------------
STACK 100h

END main
