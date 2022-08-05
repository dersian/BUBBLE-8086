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
	ARG 	@@VM:byte
	USES 	eax

	movzx ax,[@@VM]
	int 10h

	ret
ENDP setVideoMode

; Update the colour palette.
; 	* Ncolours: number of colours that have to be updated [word]
PROC updateColourPalette
	ARG	 	@@Ncolours: word
	USES 	eax, ecx, edx, esi

	mov esi, offset palette	; pointer to source palette
	movzx ecx, [@@Ncolours] ; amount of colors to read (movzx = zero extend)
	
	; multiply ecx by 3 (three color components per color)
	lea ecx, [2*ecx + ecx]	; do it efficiently 3*ecx = 2*ecx + ecx
		; ; You can also do:
		; mov eax, ecx
		; sal eax, 1
		; add ecx, eax

	mov dx, 03C8h 	; DAC write port
	xor al, al		; index of first color to change (0)
	out dx, al		; write to IO

	inc dx
	rep outsb		; update all colors

	ret
ENDP updateColourPalette

; create a gradient palette
PROC gradientPalette
	USES eax, edi

	mov edi, offset palette
	xor al, al	; start with black (0)

@@colorloop:
	stosb
	stosb
	stosb
	inc al
	cmp al, 64
	jl @@colorloop
	
	ret
ENDP gradientPalette

; draw gradient on screen
PROC drawGradient
	USES eax, ecx, edi

	mov edi, VMEMADR
	mov ecx, SCRHEIGHT	; for every horizontal line
	
@@screenloop:
	xor al, al	; start with black (0)
	push ecx
@@lineloop:
	mov ecx, SCRWIDTH/64
	rep stosb
	inc al
	cmp al, 64
	jl @@lineloop
	pop ecx
	loop @@screenloop
	
	ret
ENDP drawGradient

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

; Fill the background (for mode 13h) faster
; (uses stosd optimization)
;	* fillcolor: input color
PROC fillBackgroundFaster
	ARG 	@@fillcolor:byte
	USES 	eax, ecx, edi

	; Initialize video memory address.
	mov	edi, VMEMADR
	
	; copy color value across all bytes of eax
	mov al, [@@fillcolor]	; ???B
	mov ah, al				; ??BB
	mov cx, ax			
	shl eax, 16				; BB00
	mov ax, cx				; BBBB

	; Scan the whole video memory and assign the background colour.
	mov	ecx, SCRWIDTH*SCRHEIGHT/4
	rep	stosd

	ret
ENDP fillBackgroundFaster

; Draw a rectangle (video mode 13h)
; 	* draws the rectangle from position (x0,y0) with
;	  positive width 'w' and height 'h', with color "col"
PROC drawRectangle
	ARG 	@@x0:word, @@y0:word, @@w:word, @@h:word, @@col: byte
	USES 	eax, ecx, edx, edi ; note: MUL uses edx!

	; Compute the index of the rectangle's top left corner
	movzx eax, [@@y0]
	mov edx, SCRWIDTH
	mul edx
	add	ax, [@@x0]

	; Compute top left corner address
	mov edi, VMEMADR
	add edi, eax
	
	; Plot the top horizontal edge.
	movzx edx, [@@w]	; store width in edx for later reuse
	mov	ecx, edx
	mov	al,[@@col]
	rep stosb
	sub edi, edx		; reset edi to left-top corner
	
	; plot both vertical edges
	movzx ecx,[@@h]
	@@vertLoop:
		mov	[edi],al		; left edge
		mov	[edi+edx-1],al	; right edge
		add	edi, SCRWIDTH
		loop @@vertLoop
	; edi should point at the bottom-left corner now
	sub edi, SCRWIDTH

	; Plot the bottom horizontal edge.
	mov	ecx, edx
	rep stosb
	ret
ENDP drawRectangle

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
	
	call 	gradientPalette
	call	updateColourPalette, 64
	;call	drawGradient

	call	drawRectangle,60,60,60,60, 32
	call	drawRectangle,80,80,100,100, 63
	call	drawRectangle,1,1,SCRWIDTH-2,SCRHEIGHT-2, 48
	
	call	waitForSpecificKeystroke, 001Bh
	call	terminateProcess
ENDP main

; -------------------------------------------------------------------
DATASEG
	palette		db 768 dup (?)
; -------------------------------------------------------------------
; STACK
; -------------------------------------------------------------------
STACK 100h

END main
