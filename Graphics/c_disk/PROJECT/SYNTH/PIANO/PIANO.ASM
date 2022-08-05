; -------------------------------------------------------------------
; 80386
; 32-bit x86 assembly language
; TASM
;
; author:	David Blinder, Stijn Bettens
; date:		23/10/2018
; program:	Video mode 13h
; -------------------------------------------------------------------

IDEAL
P386
MODEL FLAT, C
ASSUME cs:_TEXT,ds:FLAT,es:FLAT,fs:FLAT,gs:FLAT

; compile-time constants (with macros)
VMEMADR EQU 0A0000h	; video memory address
SCRWIDTH EQU 320	; screen witdth
SCRHEIGHT EQU 200	; screen height

PIANOSIZE EQU 15 ; amount of bytes 

; -------------------------------------------------------------------
CODESEG

; Set the video mode
PROC setVideoMode
	ARG 	@@VM:byte
	USES 	eax

	movzx ax,[@@VM]
	int 10h

	ret
ENDP setVideoMode

; Fill the background (for mode 13h)
;	* fillcolor: input color
PROC fillBackground
	ARG 	@@fillcolor:byte
	USES 	eax, ecx, edi

	; Initialize video memory address.
	mov	edi, VMEMADR

	; Scan the whole video memory and assign the background colour.
	mov	ecx, SCRWIDTH*SCRHEIGHT
	mov	al,[@@fillcolor]
	rep	stosb

	ret
ENDP fillBackground

; opens file, returns file handle in ax
PROC openFile
	ARG @@file
	USES eax, ebx, ecx, edx
	mov al, 0 ; read only
	mov edx, [@@file]
	mov ah, 3dh
	int 21h
	
	jnc @@no_error ; carry flag is set if error occurs

	call setVideoMode, 03h
	mov  ah, 09h
	mov  edx, offset openErrorMsg
	int  21h
	
	mov	ah,00h
	int	16h
	call terminateProcess
	
@@no_error:
	mov [filehandle], ax
	ret
ENDP openFile

; reads chunk to buffer
PROC readChunk
	USES eax, ebx, ecx, edx
	mov bx, [filehandle]
	mov cx, PIANOSIZE ; amount of bytes to read
	mov edx, offset packedframe 
	mov ah, 3fh
	int 21h
	
	jnc @@no_error ; carry flag is set if error occurs

	call setVideoMode, 03h
	mov  ah, 09h
	mov  edx, offset readErrorMsg
	int  21h
	
	mov	ah,00h
	int	16h
	call terminateProcess
	
@@no_error:
	ret
ENDP readChunk

; closes file
PROC closeFile
	USES eax, ebx, ecx, edx
	mov bx, [filehandle]
	mov ah, 3Eh
	int 21h
	
	jnc @@no_error ; carry flag is set if error occurs

	call setVideoMode, 03h
	mov  ah, 09h
	mov  edx, offset closeErrorMsg
	int  21h
	
	mov	ah,00h
	int	16h
	call terminateProcess
	
@@no_error:
	ret
ENDP closeFile

PROC drawPixel
	ARG @@x:dword, @@y:dword, @@clr:byte
	USES edi, eax

	; Compute index of the pixel
	mov eax, SCRWIDTH ; initialize screen width
	mul [@@y] ; multiple eax(screen width) with the y position
	add eax, [@@x] ; add the xpos

	; Compute pixel adress
	mov edi, VMEMADR 
	add edi, eax

	; Plot the pixel
	mov al, [@@clr]
	mov [edi], al
	ret
ENDP drawPixel

PROC drawButton
	ARG 	@@xpos:dword, @@ypos:dword, @@sprite:dword, @@width:dword, @@height:dword
	USES eax, ebx, edx, edi, esi

	mov esi, [@@sprite] ; load the sprite data -> 1D array
	mov ecx, 0 ; start from first pixel;
	xor eax, eax ; set eax to 0
	xor edx, edx ; set edx to 0
	@@draw_loop:
		add edx, [@@xpos] ; x position of current pixel
		add eax, [@@ypos] ; y position of current pixel
		call drawPixel, edx, eax, [word ptr esi + ecx] ; draw the current pixel
		jmp @@next_pixel
		@@next_pixel:
			inc ecx ; increment ecx by 1 -> go to the next pixel in the sprite data
			mov edx, 0 ; set modulo to 0
			mov eax, ecx ; store value of ecx in eax so that we can divide edx by ecx(=eax)
			div [@@width] ; divide ecx(=eax) by the height -> edx = modulo
			cmp ax, [word ptr esi] ; compare modulo with the full size of the sprite
			jnz @@draw_loop ; if modulo is not zero -> jump to next row, else -> finished
	ret
ENDP drawButton

; Wait for a specific keystroke.
PROC waitForSpecificKeystroke
	ARG 	@@key:byte
	USES 	eax

	@@waitForKeystroke:
		mov	ah,00h
		int	16h
		cmp	al,[@@key]
	jne	@@waitForKeystroke

	ret
ENDP waitForSpecificKeystroke

; check for a specific termination keystroke.
PROC checkForTermination
	ARG 	@@key:byte
	USES 	eax

	mov	ah, 01h
	int	16h
	cmp	al,[@@key]
	
	jne	@@continue
	call terminateProcess
	
	@@continue:
	ret
ENDP checkForTermination

; Terminate the program.
PROC terminateProcess
	USES eax
	call setVideoMode, 03h
	mov	ax,04C00h
	int 21h
	ret
ENDP terminateProcess

PROC main
	sti
	cld
	
	push ds
	pop	es
	
	call	setVideoMode,13h
	call	fillBackground, 0

	call 	openFile, offset filename
	call	readChunk
	call	drawButton, 25, 100, offset packedframe, 15, 1
	call 	checkForTermination, 001Bh ; keycode for ESC
	call	closeFile
ENDP main

; -------------------------------------------------------------------
DATASEG
	palette		db 768 dup (?)

	filename db "sprite.bin", 0
	openErrorMsg db "could not open file", 13, 10, '$'
	readErrorMsg db "could not read data", 13, 10, '$'
	closeErrorMsg db "error during file closing", 13, 10, '$'

; -------------------------------------------------------------------
UDATASEG
	filehandle dw ?
	packedframe db PIANOSIZE dup (?)

; -------------------------------------------------------------------
; STACK
; -------------------------------------------------------------------
STACK 100h

END main
