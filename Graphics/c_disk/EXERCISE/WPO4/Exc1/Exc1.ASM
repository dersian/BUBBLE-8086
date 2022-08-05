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

; -------------------------------------------------------------------
CODESEG

; Set the video mode
PROC setVideoMode
	ARG @@mode:byte
	USES eax
	mov ah, 0
	mov al, [@@mode]
	int 10h

	ret
ENDP setVideoMode

; create a gradient palette
PROC gradientPalette
	USES eax, edi

	mov edi, offset palette
	xor al, al ; start with black(0)

@@colorloop:
	stosb
	stosb
	stosb
	inc al
	cmp al, 64
	jl @@colorloop

	ret
ENDP gradientPalette

; Update the colour palette.
PROC updateColourPalette
	ARG @@length:byte
	USES esi, ecx

	mov dx, 03c8h
	mov al, 0h 
	out dx, al
	mov dx, 03c9h 
	mov esi, offset custompalette
	mov ecx, [@@length]*3
	rep outsb

	ret
ENDP updateColourPalette

; draw gradient on screen
PROC drawGradient
	USES eax, ecx, edi

	mov edi, VMEMADR
	mov ecx, SCRHEIGHT ; for every horizontal line

@@screenloop:
	xor al, al ; start with black(0)
	push ecx

@@lineloop:
	mov ecx, SCRWIDTH/64
	rep stosb
	inc al
	cmp al, 64
	jl

	ret 
ENDP drawGradient

; Fill the background (for mode 13h)
PROC fillBackground
	ARG @@fillcolor:byte
	USES eax, ecx, edi

	mov	edi, VMEMADR
	
	; scan the whole video memory and assign the background color
	mov ecx, SCRWIDTH*SCRHEIGHT
	mov al, [@@fillcolor] ; ecx keer waarde kopieren naar al array
	rep stosb ; waarde nemen in register 

	ret
ENDP fillBackground

; ; Draw a rectangle (video mode 13h)
; PROC drawRectangle
; ...
; ENDP drawRectangle

; ; Wait for a specific keystroke.
PROC waitForSpecificKeystroke
	ARG @@esc:byte

	mov ah, [@@esc]		; wait for keystroke
    int 16h
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
	; call	fillBackground, 15
	
	call 	gradientPalette
	call	updateColourPalette, 64
	call	drawGradient
	
	; call	drawRectangle,60,60,60,60, 32
	; call	drawRectangle,80,80,100,100, 63
	; call	drawRectangle,1,1,SCRWIDTH-2,SCRHEIGHT-2, 48
	
	mov	ah,00h
	int	16h
	call waitForSpecificKeystroke, 001Bh ; keycode for ESC
	
	call terminateProcess
ENDP main

; -------------------------------------------------------------------
DATASEG
	palette		db 768 dup (?)
; -------------------------------------------------------------------
; STACK
; -------------------------------------------------------------------
STACK 100h

END main
