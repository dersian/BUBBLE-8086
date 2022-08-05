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

INCLUDE "keyb.inc"

; compile-time constants (with macros)
VMEMADR EQU 0A0000h	; video memory address
SCRWIDTH EQU 320	; screen witdth
SCRHEIGHT EQU 200	; screen height

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
	call __keyb_installKeyboardHandler

	call	drawButton, [xpos], [ypos], offset button, 24, 22

	main_loop:
		mov al, [offset __keyb_keyboardState + 39h] ; check state of space
		cmp al, 01h ; if state is 1 -> jump to function
		je space_pressed
		mov     al, [offset __keyb_keyboardState + 38h] ; 
		cmp     al, 01h
		je end_program
		jmp     main_loop 
	space_pressed:
		mov al, [offset __keyb_keyboardState + 30h] ; Space + B
		cmp al, 01h ; B also pressed -> draw
		je draw
		jmp main_loop
		draw:
			add [xpos], 50
			add [ypos], 50
			call	drawButton, [xpos], [ypos], offset button, 24, 22 ; do something, for example draw a second sprite
			; in later itterations of the project we could adjust the frequency by a key press for example
			jmp main_loop

	end_program:
		call __keyb_uninstallKeyboardHandler
		call	terminateProcess

ENDP main

; -------------------------------------------------------------------
DATASEG
	palette		db 768 dup (?)

	xpos dd 50
	ypos dd 25

	button      DB 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H
				DB 10H, 10H, 10H, 10H, 10H, 10H, 10H, 0FH, 0FH, 0FH, 0FH, 0FH, 0FH, 0FH, 0FH, 0FH, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H
				DB 10H, 10H, 10H, 10H, 10H, 10H, 0FH, 0FH, 0FH, 0FH, 0FH, 0FH, 0FH, 0FH, 0FH, 0FH, 0FH, 10H, 10H, 10H, 10H, 10H, 10H, 10H
				DB 10H, 10H, 10H, 10H, 10H, 0FH, 0FH, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 0FH, 0FH, 10H, 10H, 10H, 10H, 10H, 10H
				DB 10H, 10H, 10H, 10H, 0FH, 0FH, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 0FH, 0FH, 10H, 10H, 10H, 10H, 10H
				DB 10H, 10H, 10H, 0FH, 0FH, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 0FH, 0FH, 10H, 10H, 10H, 10H
				DB 10H, 10H, 0FH, 0FH, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 0FH, 0FH, 10H, 10H, 10H
				DB 10H, 0FH, 0FH, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 0FH, 0FH, 10H, 10H
				DB 10H, 0FH, 0FH, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 0FH, 0FH, 10H, 10H
				DB 10H, 0FH, 0FH, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 0FH, 0FH, 10H, 10H
				DB 10H, 0FH, 0FH, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 0FH, 0FH, 0FH, 0FH, 0FH, 0FH, 0FH, 0FH, 10H, 0FH, 0FH, 10H, 10H
				DB 10H, 0FH, 0FH, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 0FH, 0FH, 10H, 10H
				DB 10H, 0FH, 0FH, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 0FH, 0FH, 10H, 10H
				DB 10H, 0FH, 0FH, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 0FH, 0FH, 10H, 10H
				DB 10H, 10H, 0FH, 0FH, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 0FH, 0FH, 10H, 10H, 10H
				DB 10H, 10H, 10H, 0FH, 0FH, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 0FH, 0FH, 10H, 10H, 10H, 10H
				DB 10H, 10H, 10H, 10H, 0FH, 0FH, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 0FH, 0FH, 10H, 10H, 10H, 10H, 10H
				DB 10H, 10H, 10H, 10H, 10H, 0FH, 0FH, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 0FH, 0FH, 10H, 10H, 10H, 10H, 10H, 10H
				DB 10H, 10H, 10H, 10H, 10H, 10H, 0FH, 0FH, 0FH, 0FH, 0FH, 0FH, 0FH, 0FH, 0FH, 0FH, 0FH, 10H, 10H, 10H, 10H, 10H, 10H, 10H
				DB 10H, 10H, 10H, 10H, 10H, 10H, 10H, 0FH, 0FH, 0FH, 0FH, 0FH, 0FH, 0FH, 0FH, 0FH, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H
				DB 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H
				DB 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H, 10H
; -------------------------------------------------------------------
; STACK
; -------------------------------------------------------------------
STACK 100h

END main
