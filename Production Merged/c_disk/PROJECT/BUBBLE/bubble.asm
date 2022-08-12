IDEAL
P386
MODEL FLAT, C
ASSUME cs:_TEXT,ds:FLAT,es:FLAT,fs:FLAT,gs:FLAT

; -------------------------------------------------------------------
; MACROS + INCLUDES
; -------------------------------------------------------------------
INCLUDE "keyb.inc"


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

SHOOTBALL_STARTPOS EQU 496 ; Absolute starting position of the shooting ball
SHOOTBALL_STARTX EQU 15 ; Starting xcord of the shooting ball
SHOOTBALL_STARTTYPE EQU 1 ; Starting ball type of the shooting ball

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
; INITIALIZING
; -------------------------------------------------------------------
PROC initialize 
	call __keyb_installKeyboardHandler

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

	ret 
ENDP initialize
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
; ARRAY/UPDATE/MOVE PROCEDURES
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

; update ARRAY: place ball of given type at a given position and choose to check hit detection or not
PROC updateArray
	ARG @@arr:dword, @@position:dword, @@ball_type:dword, @@hitDetectionCheck:dword
	USES edx, eax, ebx, ecx, esi, edi
	; load in given parameters
	mov esi, [@@arr]
	xor ecx, ecx
	mov ecx, [@@position]
	mov ebx, [@@ball_type]
	; place the ball in the array
	call drawBackground, offset buffer, offset bgframe
	call timer, 2
	mov [dword ptr (esi + ecx*4)], ebx
	call decodeArray, [dword ptr esi]
	call refreshVideo
	cmp [@@hitDetectionCheck], 1
	je @@hitDetection
	jmp @@done
	
	@@hitDetection:
		mov [ball_hit], 0 ; reset global variable -> used to check if the played ball has hit any balls of the same type -> remove if yes
		;xor ecx, ecx 
		;mov ecx, edi ; put the position of the current ball in ecx
		mov edx, [dword ptr (esi + ecx*4)] ; put the type of the played ball in ebx to pass it to the hitdetection function, refer to it as a pointer in the hitdetection function
		xor edi, edi ; edi = orientation of hitdetection, clear it before passing to the function	
		; if the position is max left or max right -> skip hit detection partially
		mov edi, edx ; temp save edx in edi so we can use edx to perform the modulo operation
		xor edx, edx ; clear edx for modulo
		xor ebx, ebx
		xor eax, eax
		mov eax, ecx ; dividend
		mov ebx, 32 ; divisor
		div ebx ; divide ecx by ebx (32)
		cmp edx, 0; ecx is max left position? -> ex: 480/32 -> mod = 0
		je @@max_left
		cmp edx , 31 ; ecx is max right position -> ex: 511/32 -> mod = 31
		je @@max_right
		jmp @@normal
		
		@@normal:
			xor edx, edx
			mov edx, edi ; restore edx
			call hitDetection, ecx, 1 ; check if the right (1) balls are hit (ecx = position of newly played ball)
			call hitDetection, ecx, 2 ; 2 = left
			sub ecx, 31 ; move 1 row up and 1 to the right (32-1)
			call hitDetection, ecx, 3 ; 3 = up right
			sub ecx, 2 ; move 1 position to the left
			call hitDetection, ecx, 4 ; 4 = up left
			jmp @@check_hit_detection
		
		@@max_left: ; skip left and up left hit detection -> special case in hit detection function: 5 
			xor edx, edx
			mov edx, edi 
			call hitDetection, ecx, 5
			mov [hitDetection_state], 5
			jmp @@check_hit_detection
		
		@@max_right:
			xor edx, edx
			mov edx, edi
			call hitDetection, ecx, 6
			jmp @@check_hit_detection

		@@check_hit_detection:
			cmp [ball_hit], 1 ; check if the played ball had a succesfull hit detection
			je @@remove_played_ball ; if yes remove the played ball otherwise leave it
			jmp @@done

		@@remove_played_ball:
			cmp [hitDetection_state], 5
			je @@left
			add ecx, 33 ; reset the position to the position of the played ball
			call removeBall, ecx

			@@left:
				;add ecx, 31
				call removeBall, ecx
		

		
	@@done:
		ret
ENDP updateArray

PROC removeBall
	ARG @@removepos:dword
	USES esi, ecx
	
	mov ecx, [@@removepos]
	call drawBackground, offset buffer, offset bgframe 
	call timer, 3
	mov [dword ptr (esi + ecx*4)], 0 
	call decodeArray, [dword ptr esi] 
	call refreshVideo

	ret
ENDP removeBall

; Moves the shooting/start bal of a given type 1 position in the desired direction
PROC moveStartBall
	ARG @@arr:dword, @@orientation:dword
	USES ecx, esi
	
	mov esi, [@@arr]
	; first remove the start ball
	call removeBall, [startBall_pos]

	; check orientation and place start ball on the new position
	cmp [@@orientation], 0 ; left
	je @@move_left
	cmp [@@orientation], 1 ; right
	je @@move_right
	jmp @@done
	
	@@move_left:
		sub [startBall_pos], 1
		cmp [startBall_pos], 479 ; max left position
		je @@move_right
		mov ecx, [startBall_pos]
		call updateArray, esi, [startBall_pos], [startBall_type], 0
		jmp @@done

	@@move_right:
		add [startBall_pos], 1
		cmp [startBall_pos], 512 ; max right position
		je @@move_left
		mov ecx, [startBall_pos]
		call updateArray, esi, [startBall_pos], [startBall_type], 0
		jmp @@done
	
	@@done:
		ret
ENDP moveStartBall

PROC shootStartBall
	ARG @@arr:dword
	USES eax, ebx, ecx, edx, edi
	
	mov esi, [@@arr]
	xor ebx, ebx ; use ebx to count the amount of rows that we need to move up
	xor ecx, ecx
	mov ecx, [startBall_pos]
	jmp @@count_upper_rows

	; count the rows to go up
	@@count_upper_rows:
		sub ecx, 32
		mov edi, [dword ptr (esi + ecx*4)]
		cmp edi, 0
		ja @@check_sides ; jump if above
		inc ebx
		jmp @@count_upper_rows
	
	; check if left or right places are empty or not -> know when to shift or go a place back
	@@check_sides:
		; special cases -> if we are at the max left or max right position of the grid
		xor edx, edx ; clear edx for modulo
		xor eax, eax
		xor edi, edi
		mov eax, ecx ; dividend
		mov edi, 32 ; divisor
		div edi ; divide ecx by eax (32)
		cmp edx, 0; ecx is max left position? -> ex: 480/32 -> mod = 0
		je @@special_left
		cmp edx , 31 ; ecx is max right position -> ex: 511/32 -> mod = 31
		je @@special_right
		; other cases 
		add ecx, 33 ; right
		mov edi, [dword ptr (esi + ecx*4)]
		cmp edi, 0 
		je @@set_shift_right
		sub ecx, 2 ; left
		mov edi, [dword ptr (esi + ecx*4)]
		cmp edi, 0
		je @@set_shift_left
		sub ebx, 1
		jmp @@continue

		; 2 possibilities (for max left ex, same for max right but opposite direction):
		; case 1:
		; 	1, 0, 2
		; 	0, 2, 0
		;	| ball comes from max left position
		; 	-> we check the right and see that there is a ball so we need to move 1 row down
		; case 2:
		; 	1, 0, 2
		; 	0, 0, 0
		; 	| 
		; 	-> we check the right and see that there is no ball so we need to shift to the right
		@@special_left:
			add ecx, 33 
			mov edi, [dword ptr (esi + ecx*4)]
			cmp edi, 0 
			je @@set_shift_right
			sub ebx, 1
			jmp @@continue
		@@special_right:
			add ecx, 31
			mov edi, [dword ptr (esi + ecx*4)]
			cmp edi, 0 
			je @@set_shift_left
			sub ebx, 1
			jmp @@continue

		@@set_shift_left:
			mov edx, 1
			jmp @@continue
		@@set_shift_right:
			mov edx, 2
			jmp @@continue

	
	@@continue:
		xor ecx, ecx
		mov ecx, [startBall_pos]
		jmp @@up
		
		; move up loop
		@@up:
			call removeBall, ecx ; first remove ball
			sub ecx, 32
			call updateArray, esi, ecx, [startBall_type], 1 ; fill array with corresponding ball WITH hit detection enabled
			dec ebx
			cmp ebx, 0
			je @@check_shift
			jmp @@up

		; check if we have to shift after we went up
		@@check_shift:
			cmp edx, 1
			je @@shift_left 
			cmp edx, 2
			je @@shift_right
			jmp @@done
			
			@@shift_left:
				call removeBall, ecx
				sub ecx, 1
				call updateArray, esi, ecx, [startBall_type], 1 
				jmp @@done
			@@shift_right:
				call removeBall, ecx
				add ecx, 1
				call updateArray, esi, ecx, [startBall_type], 1 
				jmp @@done

	@@done:
		ret
ENDP shootStartBall

PROC resetStartBall
	ARG @@arr:dword
	USES eax, ebx, ecx, edx, edi
	
	mov esi, [@@arr]
	; mov [startBall_pos], RANDOM CIJFER TUSSEN 1 en 4 -> call random
	call updateArray, esi, [startBall_pos], [startBall_type], 1

	ret
ENDP resetStartBall
; -------------------------------------------------------------------
; EXTRA PROCEDURES
; -------------------------------------------------------------------
PROC hitDetection
	ARG @@hitpos:dword, @@orientation:dword
	USES eax, ebx, ecx, edx, edi

	mov ecx, [@@hitpos] ; move the passed position to the ecx register (ecx is used to acces the elements in the array on a given position)
	
	; check the orientation: left, right, up right or up left
	mov edi, [@@orientation] ; move the passed orientation to the edi register (edi is used to check the current orientation)
	mov [hitDetection_state], edi ; global variable to check the hit detection state
	cmp edi, 1
	je @@check_right
	cmp edi, 2
	je @@check_left
	cmp edi, 3
	je @@check_up_right
	cmp edi, 4
	je @@check_up_left
	cmp edi, 5
	je @@check_up_right ; max left position
	cmp edi, 6
	je @@check_up_left ; max right position
	
	@@check_right:
		add ecx, 2 ; move 1 position to the right
		mov ebx, [dword ptr (esi + ecx*4)] ; check the type of ball
		cmp ebx, edx ; compare it to the type of the played ball
		je @@remove ; remove if equal
		jmp @@done
	
	@@check_left:
		sub ecx, 2
		mov ebx, [dword ptr (esi + ecx*4)]
		cmp ebx, edx
		je @@remove
		jmp @@done
	
	@@check_up_right:
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
		mov [ball_hit], 1 ; indicate that the ball has been hit
		; check the current orientation
		cmp edi, 1
		je @@R
		cmp edi, 2
		je SHORT @@L
		cmp edi, 3
		je @@SRSL
		cmp edi, 4
		je @@SRSL
		cmp edi, 5
		je @@max_left
		cmp edi, 6
		je @@max_right

		@@R:
			; right orientation -> check the right, upright and left ball for is these are of the same type => recursive
			call hitDetection, ecx, 1 ; right
			call removeBall, ecx ; remove the ball because it was of the same type as the played ball
			sub ecx, 31 
			call hitDetection, ecx, 3 ; up right
			sub ecx, 2
			call hitDetection, ecx, 4 ; up left
			jmp @@done 
		
		@@L:
			; left orientation -> check the left, upright and left ball
			call hitDetection, ecx, 2
			call removeBall, ecx 
			sub ecx, 31 
			call hitDetection, ecx, 3 
			sub ecx, 2
			call hitDetection, ecx, 4 
			jmp SHORT @@done

		@@SRSL:
			; up right and up left orientation -> check the left, right, upright and upleft for same type -> we check the left and right orientation because we moved 1 row up
			call hitDetection, ecx, 1 
			call hitDetection, ecx, 2 
			call removeBall, ecx 
			sub ecx, 31 
			call hitDetection, ecx, 3 
			sub ecx, 2
			call hitDetection, ecx, 4 
			jmp @@done
		
		@@max_left:
			; max left orientation -> only check right and up right
			call hitDetection, ecx, 1 
			sub ecx, 31 
			call hitDetection, ecx, 3
			jmp @@done
		
		@@max_right:
			; max right orientation -> only check left and upper left
			call hitDetection, ecx, 2 
			sub ecx, 31
			sub ecx, 2 
			call hitDetection, ecx, 4
			jmp @@done
			

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
	
	call initialize

	; initialize starting parameters
	mov [startBall_pos], SHOOTBALL_STARTPOS
	mov [startBall_type], SHOOTBALL_STARTTYPE
	call updateArray, offset arr_screen, [startBall_pos], 1, 0 ; place the startball on the grid
	
	@@keyboardLoop:
		mov al, [__keyb_rawScanCode] ;last pressed key
		cmp al, 4Bh ; left arow key
		je @@moveStartBallLeft
		cmp al, 4Dh ; right arrow key
		je @@moveStartBallRight
		cmp al, 39h ; space key
		je @@shootStartBall
		cmp al, 01h ; escape key
		je @@exit
		jmp @@keyboardLoop

		@@moveStartBallLeft:
			call moveStartBall, offset arr_screen, 0
			jmp @@keyboardLoop
		@@moveStartBallRight:
			call moveStartBall, offset arr_screen, 1
			jmp @@keyboardLoop
		@@shootStartBall:
			call shootStartBall, offset arr_screen
			call resetStartBall, offset arr_screen
			jmp @@keyboardLoop

	
	@@exit:
		call refreshVideo

		call __keyb_uninstallKeyboardHandler

		;call	waitForSpecificKeystroke, 001Bh
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
	
	ball_hit dd 0
	hitDetection_state dd 0
	
	startBall_pos dd 0
	startBall_type dd 0

	upper_rows dd 0

	; Current Screen Buffer(32x16) (0's represent space between balls)
	arr_screen 	dd 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0 ; row 1
				dd 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1 ; row 2
				dd 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0 ; row 3
				dd 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 2 ; row 4
				dd 2, 0, 1, 0, 2, 0, 2, 0, 2, 0, 2, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 2, 0 ; row 5
				dd 0, 2, 0, 2, 0, 0, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2 ; row 6
				dd 2, 0, 2, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 2, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 2, 0 ; row 7
				dd 0, 1, 0, 0, 0, 0, 0, 2, 0, 2, 0, 2, 0, 2, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 2, 0, 1 ; row 8
				dd 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ; row 9
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