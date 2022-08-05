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
	ARG @@VM:byte ;het argument VM is een bite bvb: 13h
	USES eax

	movzx ax, [@@VM] ;movezx als je een 8 bit getal bebruikt (mov is 16bit)
	int 10h
	ret
ENDP setVideoMode

; create a gradient palette
; PROC gradientPalette
; ...
; ENDP gradientPalette

; ; Update the colour palette.
; PROC updateColourPalette
; ...
; ENDP updateColourPalette

; draw gradient on screen
PROC drawGradient
	
	ret
ENDP drawGradient

;Fill the background (for mode 13h)
PROC fillBackground
	ARG	@@fillcolor:byte
	USES eax, ecx, EDI
	
	mov edi, VMEMADR ; pointer for videoadress
	MOV ecx, SCRWIDTH*SCRHEIGHT
	mov al, [@@fillcolor]
	rep stosb
	ret
	
	
	
	; cld
	; mov ESI, offset palette ; set the palette (DAC) address
	; mov ECX, 2*3 ; set color 0 and 1
	; 			; (2 indexes in total, 2 * 3 bytes)
	; mov DX, 03C8h ; VGA DAC set port
	; mov AL, 0 ; set start color index
	; out DX, AL ; signal index 0
	; inc DX ; DAC data port (03C9h = 03C8h + 1)
	; rep outsb
	; ret



	; ;ARG @@COLOR:int
	; mov EDI, 0A0000h
	; mov AL, 15
	; mov [EDI],AL
	; add EDI, 2*230+10
	; mov [EDI], AL
	; ret

ENDP fillBackground

; ; Draw a rectangle (video mode 13h)
; PROC drawRectangle
; ...
; ENDP drawRectangle

; ; Wait for a specific keystroke.
; PROC waitForSpecificKeystroke
; ...
; ENDP waitForSpecificKeystroke

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
	call	fillBackground, 6*15+5 ;Figure 5.2 Compendium
	
	; call 	gradientPalette
	; call	updateColourPalette, 64
	; call	drawGradient
	
	; call	drawRectangle,60,60,60,60, 32
	; call	drawRectangle,80,80,100,100, 63
	; call	drawRectangle,1,1,SCRWIDTH-2,SCRHEIGHT-2, 48
	
	mov	ah,00h
	int	16h
	; (replace by) call	waitForSpecificKeystroke, 001Bh ; keycode for ESC
	
	call terminateProcess
ENDP main

; -------------------------------------------------------------------
DATASEG
	; palette		db 768 dup (?)
	palette db 255, 0, 0, 63, 63, 63
; -------------------------------------------------------------------
; STACK
; -------------------------------------------------------------------
STACK 100h

END main
