; -------------------------------------------------------------------
; 80386
; 32-bit x86 assembly language
; TASM
;
; author:	David Blinder
; date:		1/12/2017
; program:	Keyboard example
; -------------------------------------------------------------------

IDEAL
P386
MODEL FLAT, C
ASSUME cs:_TEXT,ds:FLAT,es:FLAT,fs:FLAT,gs:FLAT

;INCLUDE "keyb.inc"

; compile-time constants (with macros)
VMEMADR EQU 0A0000h	; video memory address
SCRWIDTH EQU 320	; screen width
SCRHEIGHT EQU 200	; screen height

FRAMEWIDTH EQU 80
FRAMEHEIGHT EQU 50	; frame height and width (in pixels)
FRAMESIZE EQU FRAMEHEIGHT*FRAMEWIDTH/4

FRAMECOUNT EQU 288

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

; Update the colour palette.
; 	* Ncolours: number of colours that have to be updated [word]
PROC updateColourPalette
	ARG	 	@@Ncolours: word
	USES 	eax, ecx, edx, esi

	mov esi, offset palette	; pointer to source palette
	movzx ecx, [@@Ncolours] ; amount of colors to read (movzx = zero extend)
	
	; multiply ecx by 3 (three color components per color)
	; do it efficiently (2*ecx + ecx)
	mov eax, ecx
	sal eax, 1
	add ecx, eax

	mov dx, 03C8h 	; DAC write port
	xor al, al		; index of first color to change (0)
	out dx, al		; write to IO

	inc dx
	rep outsb		; update all colors

	ret
ENDP updateColourPalette

; Terminate the program.
PROC terminateProcess
	USES eax
	call setVideoMode, 03h
	mov	ax,04C00h
	int 21h
	ret
ENDP terminateProcess

; wait for @@framecount frames
proc wait_VBLANK
	ARG @@framecount: word
	USES eax, ecx, edx
	mov dx, 03dah 					; Wait for screen refresh
	movzx ecx, [@@framecount]
	
		@@VBlank_phase1:
		in al, dx 
		and al, 8
		jnz @@VBlank_phase1
		@@VBlank_phase2:
		in al, dx 
		and al, 8
		jz @@VBlank_phase2
	loop @@VBlank_phase1
	
	ret 
endp wait_VBLANK
; -------------------------------------------------------------------

; opens file, returns file handle in ax
PROC openFile
	USES eax, ebx, ecx, edx
	mov al, 0 ; read only
	mov edx, offset filename
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
	mov cx, FRAMESIZE
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

; -------------------------------------------------------------------

; Expand the packed frame by a factor of four
PROC expandPackedFrame
	ARG	 	@@destptr: dword	; destination buffer
	USES 	eax, ebx, ecx, edx, edi, esi

	mov ebx, offset packedframe		; source pointer
	mov edi, [@@destptr]			; destination pointer
	
	; create groups of 4 scanlines each
	mov ecx, FRAMEHEIGHT	
	@@scangroup:
	push ecx
		; expand scanline
		mov ecx, FRAMEWIDTH/16	; pixels per dword per packed scanline
		@@unpackline:
		push ecx
			; unpack double word into 16 pixels
			mov ecx, 16		; pixels per packed dword
			mov esi, [ebx]	; bitcycle buffer
			@@unpackdw:
				mov eax, esi
				and eax, 3	; extract two low-end bits
				
				mov ah, al
				mov dx, ax
				shl eax, 16
				mov ax, dx	; replicate byte 4 times
				stosd ; store four consecutive pixels
				
				ror esi, 2	; cycle next 2 bits
			loop @@unpackdw
			add ebx, 4	; goto next dword
		pop ecx
		loop @@unpackline

		; replicate scanline four times
		mov esi, edi
		sub esi, SCRWIDTH	; ESI points to just inserted scanline
		mov ecx, 3*SCRWIDTH/4	; dwords in 3 scanlines
		rep movsd			; copy 3 scanlines
	pop ecx
	loop @@scangroup

	ret
ENDP expandPackedFrame


PROC main
	sti
	cld
	
	push ds
	pop	es

	call setVideoMode, 13h
	call updateColourPalette, 4
	
	@@animation_cycle:
		call openFile
		mov ecx, FRAMECOUNT
		@@animation_loop:
			call readChunk
			call expandPackedFrame, VMEMADR
			call wait_VBLANK, 3
			call checkForTermination, 001Bh ; keycode for ESC
		loop @@animation_loop
		call closeFile
	jmp @@animation_cycle

ENDP main

; -------------------------------------------------------------------
DATASEG
	palette		db 0, 0, 0
				db 10, 0, 46
				db 54, 16, 24
				db 63, 63, 63
				
	filename db "dancer.bin", 0
	openErrorMsg db "could not open file", 13, 10, '$'
	readErrorMsg db "could not read data", 13, 10, '$'
	closeErrorMsg db "error during file closing", 13, 10, '$'
	

; -------------------------------------------------------------------
UDATASEG
	filehandle dw ?
	packedframe db FRAMESIZE dup (?)

; -------------------------------------------------------------------
STACK 100h

END main
