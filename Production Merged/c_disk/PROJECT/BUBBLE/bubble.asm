IDEAL
P386
MODEL FLAT, C
ASSUME cs:_TEXT,ds:FLAT,es:FLAT,fs:FLAT,gs:FLAT

; -------------------------------------------------------------------
; MACROS + INCLUDES
; -------------------------------------------------------------------
VMEMADR EQU 0A0000h	; video memory address
FRAMEWIDTH EQU 320	; screen witdth
FRAMEHEIGHT EQU 200	; screen height
FRAMESIZE EQU FRAMEHEIGHT*FRAMEWIDTH

ARRWIDTH EQU 32
ARRHEIGHT EQU 16
ARRLEN EQU ARRWIDTH*ARRHEIGHT

XOFFSET EQU 6
YOFFSET EQU 10

BALLWIDTH EQU 8 	
BALLHEIGHT EQU 8
BALLSIZE EQU BALLHEIGHT*BALLWIDTH

BALL_XSTART EQU 64
BALL_YSTART EQU 5

SHOOTBALL_STARTPOS EQU 496 ; Absolute startpos
SHOOTBALL_STARTX EQU 15 ; Xcord of startpos

INCLUDE "keyb.inc" 

; -------------------------------------------------------------------
; CODESEG
; -------------------------------------------------------------------
CODESEG
; -------------------------------------------------------------------
; STRUCTS
; -------------------------------------------------------------------
STRUC ball
	x dd 64 ; width of 192
	y dd 5
ENDS ball
; -------------------------------------------------------------------
; VIDEO MODE
; -------------------------------------------------------------------
PROC setVideoMode
	ARG 	@@VM:byte
	USES 	eax

	movzx ax,[@@VM]
	int 10h

	ret
ENDP setVideoMode
; -------------------------------------------------------------------
; REFRESH VIDEO
; -------------------------------------------------------------------
PROC refreshVideo
	USES ecx, edi, esi

	cld
	mov esi, offset buffer
	mov edi, VMEMADR
	mov ecx, FRAMESIZE  
	rep movsb

	ret
ENDP refreshVideo
; -------------------------------------------------------------------
; BACKGROUND
; -------------------------------------------------------------------
PROC fillBackground
	ARG 	@@fillcolor:byte
	USES 	eax, ecx, edi

	; Initialize video memory address.
	mov	edi, VMEMADR

	; Scan the whole video memory and assign the background colour.
	mov	ecx, FRAMEHEIGHT*FRAMEWIDTH
	mov	al, [@@fillcolor]
	rep	stosb

	ret
ENDP fillBackground
; -------------------------------------------------------------------
; COLOUR PALETTE UPDATE
; -------------------------------------------------------------------
; 	* Ncolours: number of colours that have to be updated [word]
PROC updateColourPalette
	ARG	 	@@Ncolours: word, @@palette: dword
	USES 	eax, ecx, edx, esi

	mov esi, [@@palette]	; pointer to source palette
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
; -------------------------------------------------------------------
; RANDOM
; -------------------------------------------------------------------

; -------------------------------------------------------------------
; TIMING
; -------------------------------------------------------------------
; constant timer (DANCER reference)
PROC timer
	ARG @@framecount:word
	USES edx,ecx,eax, edi, esi
	
	movzx ecx, [@@framecount]
	mov dx, 03dah ; 
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
ENDP timer

; -------------------------------------------------------------------
; CHECK KEYBOARD
; -------------------------------------------------------------------
; read key
PROC readKey
	USES eax
	mov al, 0
	mov al, [__keyb_rawScanCode] 		; last pressed key
	cmp al, 01h							; esc key
	jne @@noPres 
	call terminateProcess
		
	@@noPres:
	ret
ENDP readKey

; wait for specific keystroke
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
; -------------------------------------------------------------------
; STARTING SCREEN
; -------------------------------------------------------------------

; -------------------------------------------------------------------
; DRAWING PROCEDURES
; -------------------------------------------------------------------
; draw background
PROC drawBackground
	ARG @@destptr:dword, @@packedframe: dword
	USES eax, ebx, ecx, edi 
	
	mov ebx, [@@packedframe]		; source pointer
	mov edi, [@@destptr]			; destination pointer
	
	mov ecx, FRAMESIZE				; amount of pixels in ecx
	@@printPixel:					
		add ebx,1
		mov	al,[ebx]
		stosb
		loop @@printPixel
	ret 
ENDP drawBackground

; draw ball
PROC drawBall
	ARG @@packedframe:dword
	USES eax, ebx, ecx, edx, edi
	
	xor ebx, ebx
	xor edx, edx
	xor eax, eax
	xor ecx, ecx
	xor edi, edi

	mov ebx, [@@packedframe]	; source pointer -> bin file 
	
	mov eax, [f.y] ; y location 
	xor edx, edx
	mov edx, FRAMEWIDTH 
	mul edx ; scale y location with frame width length
	add eax, [f.x] ; x location 
	mov edi, offset buffer
	add edi, eax				; destination pointer 
	
	mov ecx, BALLHEIGHT
@@screenloop:
	push ecx
	mov ecx, BALLWIDTH
	@@printLine:
		mov	al, [ebx] ; index in the colour palette
		cmp al, 4
		je @@skip
		jmp @@print
		@@skip:
		inc edi
		jmp @@end
		@@print:
		stosb ; store pixel
		@@end:
		add ebx, 1 ; increment
		loop @@printLine
	mov edx, FRAMEWIDTH-BALLWIDTH		; move one row down in the video memory
	add edi, edx
	pop ecx
	loop @@screenloop
	
	ret 
ENDP drawBall

; -------------------------------------------------------------------
; ARRAY/UPDATE PROCEDURES
; -------------------------------------------------------------------
; decode ARRAY
PROC decodeArray
	ARG @@arr:dword
	USES eax, ecx, esi, edx, ebx

	;mov esi, [@@arr] ; store array (already stored in update proc)
	mov ecx, 0
	; reset cord
	mov [f.x], BALL_XSTART
	mov [f.y], BALL_YSTART
	@@loopi:
		;check if we are at then end of the array
		cmp ecx, ARRLEN
		je @@done
		;check if we are at the end of the row
		mov ebx, ARRWIDTH
		mov edx, 0 ;clear edx register before division
		mov eax, ecx
		div ebx
		cmp dx, 0
		;cmp ecx, ARRWIDTH ; check if we are at the end of the x-length
		je @@nextrow
		@@cont:
			;calculate coordinates
			add [f.x], XOFFSET ; set x position
			;check 0 or 1
			mov edx, [dword ptr (esi + ecx*4)] ; copy array data at index ecx to edx
			inc ecx ; increment ecx
			cmp edx, 0 ; compare array data -> decide not to print or color
			je @@loopi ; if zero, skip 
			cmp edx, 1 ; draw blue ball
			je @@drawBlue
			cmp edx, 2 ; draw green ball
			je @@drawGreen
			cmp edx, 3 ; draw pink ball
			je @@drawPink
			cmp edx, 4 ; draw yellow ball
			je @@drawYellow
			jmp @@loopi

	@@nextrow:
		; reset x cord
		mov [f.x], BALL_XSTART
		; move y cord 1 row down(=10px)
		add [f.y], YOFFSET
		jmp @@cont
	@@drawBlue:
		call drawBall, offset blueballframe
		jmp @@loopi
	@@drawGreen:
		call drawBall, offset greenballframe
		jmp @@loopi
	@@drawPink:
		call drawBall, offset pinkballframe
		jmp @@loopi
	@@drawYellow:
		call drawBall, offset yellowballframe
		jmp @@loopi
	@@done:
		ret
ENDP decodeArray

; update ARRAY with corresponding path per timer count (simple ball movement)
PROC updateArray
	ARG @@arr:dword, @@path:dword
	USES edx, eax, ebx, ecx, esi, edi

	mov esi, [@@arr] ; store array
	mov edi, SHOOTBALL_STARTPOS ; load in start position of the shooting ball
	mov ebx, [@@path] ; load in given path
	xor ecx, ecx
	xor eax, eax

	; movement orientation
	cmp ebx, SHOOTBALL_STARTX ; 1 left from center
	je @@up ; ebx = 16? -> recht naar boven
	jg @@right ; ebx > 16? -> rechts
	jmp @@left ; ebx < 16? -> links
	
	@@up:
		call drawBackground, offset buffer, offset bgframe 
		call timer, 8
		inc ecx ; increase loop counter
		mov [dword ptr (esi + edi*4)], 0 
		sub edi, 32 
		mov [dword ptr (esi + edi*4)], 1 
		call decodeArray, [dword ptr esi] 
		call refreshVideo
		cmp ecx, 7
		je @@hit_detection
		jmp @@up

	@@right:
		;calculate amount of steps to take, #steps=counter(ecx)
		mov ecx, SHOOTBALL_STARTX ; 
		sub ebx, ecx
		mov ecx, ebx
		; start with sideways movement
		jmp @@right_sideways
		
		@@right_sideways:
			call drawBackground, offset buffer, offset bgframe 
			call timer, 8
			mov [dword ptr (esi + edi*4)], 0 
			add edi, 1
			mov [dword ptr (esi + edi*4)], 1 
			call decodeArray, [dword ptr esi] 
			call refreshVideo
			dec ecx
			cmp ecx, 0
			je @@up
			jmp @@right_sideways

	@@left:
		mov ecx, SHOOTBALL_STARTX ; 
		sub ecx, ebx
		jmp @@left_sideways
		
		@@left_sideways:
			call drawBackground, offset buffer, offset bgframe 
			call timer, 8
			mov [dword ptr (esi + edi*4)], 0 
			sub edi, 1
			mov [dword ptr (esi + edi*4)], 1 
			call decodeArray, [dword ptr esi] 
			call refreshVideo
			dec ecx
			cmp ecx, 0
			je @@up
			jmp @@left_sideways

	@@hit_detection:
		xor ecx, ecx
		mov ecx, edi ; put the position of the current ball in ecx
		mov edx, [dword ptr (esi + ecx*4)] ; put the type of the original ball in ebx to pass it to the hitdetection function, refer to it as a pointer in the hitdetection function
		xor eax, eax ; state counter
		xor edi, edi ; edi = orientation of hitdetection, clear it before passing to the function
		call hitDetection, ecx, 1 ; check if the right (1) balls are hit (ecx = position of newly played ball)
		call hitDetection, ecx, 2 ; 2 = left
		sub ecx, 31 ; 
		call hitDetection, ecx, 3 ; 3 = up right
		sub ecx, 2
		call hitDetection, ecx, 4 ; 4 = up left
		
		; First check orientation 1 (right) and 2 (left) of the hitt bal
		; Check orientation 3 (up right) -> 2 possible output states: 
		; eax = 5: SR = 0, SL = UNDEF
		; eax = 6: SR = 1, SL = UNDEF
		; Check orientation 4 (up left) -> 4 possible output states: 
		; eax = 1: SR = 1, SL = 1
		; eax = 2: SR = 1, SL = 0
		; eax = 3: SR = 0, SL = 1
		; eax = 4; SR = 0, SL = 0

		jmp @@done
		
		;@itterate:
		;	cmp [state], 1
		;	je @@hit_detection_state_1 ; SR = 1, SL = 1
		;	cmp [state], 2
		;	je @@hit_detection_state_2 ; SR = 1, SL = 0
		;	cmp [state], 3
		;	je @@hit_detection_state_3 ; SR = 0, SL = 1
		;	cmp [state], 4
		;	je @@done ; SR = 0, SL = 0

		;	@@hit_detection_state_1:
		;		; SR
		;		add ecx, 2 ; go back to SR
		;		;call hitDetection, ecx, 1 
		;		;call hitDetection, ecx, 2 
		;		sub ecx, 31 ; 
		;		call hitDetection, ecx, 3 
		;		sub ecx, 2
		;		call hitDetection, ecx, 4 
		;		; SL
		;		add ecx, 33
		;		;call hitDetection, ecx, 1 
		;		;call hitDetection, ecx, 2 
		;		sub ecx, 31 ; 
		;		call hitDetection, ecx, 3 
		;		sub ecx, 2
		;		call hitDetection, ecx, 4 
		;		jmp @@itterate				
		;	@@hit_detection_state_2:
		;		add ecx, 2
		;		call hitDetection, ecx, 1 
		;		call hitDetection, ecx, 2 
		;		sub ecx, 31 ; 
		;		call hitDetection, ecx, 3 
		;		sub ecx, 2
		;		call hitDetection, ecx, 4 
		;		jmp @@itterate
		;	@@hit_detection_state_3:
		;		call hitDetection, ecx, 1 
		;		call hitDetection, ecx, 2 
		;		sub ecx, 31 ; 
		;		call hitDetection, ecx, 3 
		;		sub ecx, 2
		;		call hitDetection, ecx, 4 
		;		jmp @@itterate

	@@done:
		ret
ENDP updateArray

PROC removeBall
	ARG @@removepos:dword
	USES esi, ecx
	
	mov ecx, [@@removepos]
	call drawBackground, offset buffer, offset bgframe 
	call timer, 8
	mov [dword ptr (esi + ecx*4)], 0 
	call decodeArray, [dword ptr esi] 
	call refreshVideo

	ret
ENDP removeBall
; -------------------------------------------------------------------
; EXTRA PROCEDURES
; -------------------------------------------------------------------
PROC hitDetection
	ARG @@hitpos:dword, @@orientation:dword
	USES eax, ebx, ecx, edx, edi

	;check the type of ball at the given position, edx = type of ball at given position
	mov ecx, [@@hitpos]
	;xor edx, edx
	;mov edx, [@@original_type]
	
	; check the orientation: left or right
	mov edi, [@@orientation]
	cmp edi, 1
	je @@check_right
	cmp edi, 2
	je @@check_left
	cmp edi, 3
	je @@check_up_right
	cmp edi, 4
	je @@check_up_left
	
	@@check_right:
		add ecx, 2
		mov ebx, [dword ptr (esi + ecx*4)]
		cmp ebx, edx
		je @@remove
		jmp @@done
	
	@@check_left:
		sub ecx, 2
		mov ebx, [dword ptr (esi + ecx*4)]
		cmp ebx, edx
		je @@remove
		jmp @@done
	
	@@check_up_right:
		; check the type of the up right ball
		; if it is of the same type as the original ball -> check right 
		mov ebx, [dword ptr (esi + ecx*4)]
		cmp ebx, edx
		je @@remove 
		jmp @@done 		

	@@check_up_left:
		mov ebx, [dword ptr (esi + ecx*4)]
		cmp ebx, edx
		je @@remove 
		jmp @@done

	@@remove:
		;call hitDetection, ecx, 1 ; check if the right (1) balls are hit (ecx = position of newly played ball)
		;call hitDetection, ecx, 2 ; 2 = left
		;sub ecx, 31 
		;call hitDetection, ecx, 3 ; 3 = up right
		;;call removeBall, ecx
		;sub ecx, 2
		;call hitDetection, ecx, 4 ; 4 = up left
		;call removeBall, ecx 
		
		cmp edi, 1
		je @@R
		cmp edi, 2
		je @@L
		cmp edi, 3
		je @@SRSL
		cmp edi, 4
		je @@SRSL
	

		@@SRSL:
			call hitDetection, ecx, 1 ; check if the right (1) balls are hit (ecx = position of newly played ball)
			call hitDetection, ecx, 2 ; 2 = left
			;call removeBall, ecx 
			sub ecx, 31 
			call hitDetection, ecx, 3 ; 3 = up right
			sub ecx, 2
			call hitDetection, ecx, 4 ; 4 = up left
			call removeBall, ecx
			jmp @@done
		
		@@R:
			call hitDetection, ecx, 1
			;call removeBall, ecx 
			sub ecx, 31 
			call hitDetection, ecx, 3 
			sub ecx, 2
			call hitDetection, ecx, 4 
			call removeBall, ecx ; remove ball after recursive function otherwise the ball on ecx would already be removed and the cmp between ecx and ebx would be false
			je @@check_right
		
		@@L:
			call hitDetection, ecx, 2
			;call removeBall, ecx 
			sub ecx, 31 
			call hitDetection, ecx, 3 
			sub ecx, 2
			call hitDetection, ecx, 4 
			call removeBall, ecx ; remove ball after recursive function otherwise the ball on ecx would already be removed and the cmp between ecx and ebx would be false
			je @@check_left

	@@done:
		ret
ENDP hitDetection
; -------------------------------------------------------------------
; SCORE PROCEDURES
; -------------------------------------------------------------------

; -------------------------------------------------------------------
; FILE PROCEDURES
; -------------------------------------------------------------------
; OPEN FILE
PROC openFile
	ARG @@filename: dword, @@filehandle: dword
	USES eax, ebx, ecx, edx, esi

	mov al, 0 ; read only
	mov edx, [@@filename]
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
	mov ebx, [@@filehandle]
	mov esi, [ebx]
	mov [esi], ax
	ret
ENDP openFile

; READ CHUNK
PROC readChunk
	ARG @@size: dword, @@filehandle: dword, @@fileframe: dword
	USES eax, ebx, ecx, edx, esi
	mov ebx, [@@filehandle]
	mov esi, [ebx]
	mov bx, [word esi]
	mov ecx, [@@size]
	mov edx, [@@fileframe]
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

; CLOSE FILE
PROC closeFile
	ARG @@filehandle: dword 
	USES eax, ebx, ecx, edx, esi
	mov ebx, [@@filehandle]
	mov esi, [ebx]
	mov bx, [word esi]
	mov ah, 3Eh
	int 21h
	jnc @@no_error ; carry flag is set if error occurs

	call setVideoMode, 03h
	mov  ah, 09h
	mov  edx, offset closeErrorMsg
	int  21h
	
@@no_error:
	ret
ENDP closeFile

; -------------------------------------------------------------------
; TERMINATION
; -------------------------------------------------------------------
PROC terminateProcess
	USES eax
	call 	closeFile, offset blueballhandle
	call 	closeFile, offset greenballhandle
	call 	closeFile, offset pinkballhandle
	call 	closeFile, offset yellowballhandle
	call 	setVideoMode, 03h
	mov	ax,04C00h
	int 21h
	
	ret
ENDP terminateProcess

; -------------------------------------------------------------------
; MAIN
; -------------------------------------------------------------------
PROC main
	sti
	cld
	
	push ds
	pop	es

	call setVideoMode, 13h
	call updateColourPalette, 54 , offset palette
	;open, read and draw background
	call openFile, offset bgfile, offset bghandle
	call readChunk, FRAMESIZE, offset bghandle, offset bgframe
	call drawBackground, offset buffer, offset bgframe
	;open, read and different balls
	call openFile, offset blueballfile, offset blueballhandle
	call readChunk, BALLSIZE, offset blueballhandle, offset blueballframe
	call openFile, offset greenballfile, offset greenballhandle
	call readChunk, BALLSIZE, offset greenballhandle, offset greenballframe
	call openFile, offset pinkballfile, offset pinkballhandle
	call readChunk, BALLSIZE, offset pinkballhandle, offset pinkballframe
	call openFile, offset yellowballfile, offset yellowballhandle
	call readChunk, BALLSIZE, offset yellowballhandle, offset yellowballframe

	;call decodeArray, offset arr_screen
	call updateArray, offset arr_screen, 5

	call refreshVideo

	call	waitForSpecificKeystroke, 001Bh
	call	terminateProcess


ENDP main

; -------------------------------------------------------------------
; UDATASEG
; -------------------------------------------------------------------
UDATASEG		
	blueballhandle dw ?
	greenballhandle dw ?
	pinkballhandle dw ?
	yellowballhandle dw ?
	bghandle dw ?

	blueballframe db BALLSIZE dup (?)
	greenballframe db BALLSIZE dup (?)
	pinkballframe db BALLSIZE dup (?)
	yellowballframe db BALLSIZE dup (?)
	bgframe db FRAMESIZE dup (?)
	
	buffer db FRAMESIZE dup (?)
; -------------------------------------------------------------------
; DATA
; -------------------------------------------------------------------
DATASEG
    ; Vars
    f ball <,,>
	
	state dd 0

	; Current Screen Buffer(32x16) (0's represent space between balls)
	arr_screen 	dd 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0 ; row 1
				dd 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1 ; row 2
				dd 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0 ; row 3
				dd 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1 ; row 4
				dd 1, 0, 1, 0, 2, 0, 2, 0, 2, 0, 2, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0 ; row 5
				dd 0, 2, 0, 2, 0, 1, 0, 1, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2 ; row 6
				dd 2, 0, 2, 0, 1, 0, 2, 0, 1, 0, 2, 0, 1, 0, 2, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0 ; row 7
				dd 0, 2, 0, 2, 0, 1, 0, 1, 0, 2, 0, 2, 0, 2, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1 ; row 8
				dd 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ; row 9
				dd 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ; row 10
				dd 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ; row 11
				dd 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ; row 12
				dd 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ; row 13
				dd 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ; row 14
				dd 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ; row 15
				dd 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ; row 16

    ;Color Palette
	palette	db 0,9,13
			db 37,42,43
			db 5,24,36
			db 59,59,59
			db 1,5,11
			db 4,34,56
			db 2,20,42
			db 21,52,61
			db 0,0,0
			db 0,0,0
			db 0,0,0
			db 0,0,0
			db 0,0,0
			db 0,0,0
			db 0,0,0
			db 0,0,0
			db 0,0,0
			db 0,0,0
			db 0,0,0
			db 0,0,0
			db 13,6,1
			db 56,34,4
			db 43,21,3
			db 61,52,21
			db 0,0,0
			db 0,0,0
			db 0,0,0
			db 0,0,0
			db 0,0,0
			db 0,0,0
			db 0,0,0
			db 0,0,0
			db 0,0,0
			db 0,0,0
			db 0,0,0
			db 0,0,0
			db 8,1,6
			db 54,4,43
			db 61,21,60
			db 35,2,23
			db 0,0,0
			db 0,0,0
			db 0,0,0
			db 0,0,0
			db 0,0,0
			db 0,0,0
			db 0,0,0
			db 0,0,0
			db 0,0,0
			db 0,0,0
			db 0,0,0
			db 0,0,0
			db 4,4,1
			db 34,39,2
			db 56,53,8
			db 21,23,2

    ; Files
	bgfile				db "bg.bin", 0
	blueballfile		db "blueball.bin", 0
	greenballfile		db "blueball.bin", 0
	yellowballfile		db "blueball.bin", 0
	pinkballfile		db "blueball.bin", 0

    ; Error Messages
	openErrorMsg 	db "could not open file", 13, 10, '$'
	readErrorMsg 	db "could not read data", 13, 10, '$'
	closeErrorMsg 	db "error during file closing", 13, 10, '$'
	
    ; Game Messages

	
; -------------------------------------------------------------------
; STACK
; -------------------------------------------------------------------
STACK 100h

END main