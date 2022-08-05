; -------------------------------------------------------------------
; 80386
; 32-bit x86 assembly language
; TASM
;
; author:	Tibo Martens, Ebe Coomans 
; date:		23/10/2021
; program:	Video mode 13h
; -------------------------------------------------------------------

IDEAL
P386
MODEL FLAT, C
ASSUME cs:_TEXT,ds:FLAT,es:FLAT,fs:FLAT,gs:FLAT

; compile-time constants (with macros)
VMEMADR EQU 0A0000h	; video memory address
FRAMEWIDTH EQU 320	; screen witdth
FRAMEHEIGHT EQU 200	; screen height
FRAMESIZE EQU FRAMEHEIGHT*FRAMEWIDTH

FLAPPYWIDTH EQU 20 	; width of the flappy
FLAPPYHEIGHT EQU 20	;height of the flappy
FLAPPYSIZE EQU FLAPPYHEIGHT*FLAPPYWIDTH

LIFEWIDTH EQU 20	; width of the life
LIFEHEIGHT EQU 20	;height of the life
LIFESIZE EQU LIFEHEIGHT*LIFEWIDTH

BOOSTWIDTH EQU 15	; width of the boost
BOOSTHEIGHT EQU 15	;height of the boost
BOOSTSIZE EQU BOOSTHEIGHT*BOOSTWIDTH

INCLUDE "keyb.inc" 
INCLUDE "rand.inc" 

; -------------------------------------------------------------------
CODESEG
; -------------------------------------------------------------------
; USED STRUCTS
STRUC flappy
	x dd 160
	y dd 100
	draw dd 0
ENDS flappy

STRUC tube
	y dd 75
	h dd 70
	x dd 300
	b dd 15
	tester dd 0		;to define if its going up or down
	color db 30h
ENDS tube

; -------------------------------------------------------------------
; Procedure to set the video mode
PROC setVideoMode
	ARG @@arg:byte
	uses eax
	
	movzx ax, [@@arg]
	int 10h
ret
ENDP setVideoMode

; -------------------------------------------------------------------
; Update the colour palette.
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
; PROCEDURES USED TO GENERATE RANDOM FUNCTIONS (comment: by using remainder the upper bound of the random number can be determined)
PROC random
	ARG @@x:dword
	USES eax,ebx
	mov ebx,[@@x]
	call rand_init
	call rand16
	call remainder, eax,ebx
	ret
ENDP random

PROC remainder
	ARG @@x:dword, @@y:dword
	USES edx,eax
	xor edx,edx
	mov eax, [@@x]
	mov ecx,[@@y]
	div ecx
	mov ecx,edx
	ret
ENDP remainder

; -------------------------------------------------------------------
;procedure to have consistent timing for the game
PROC time
	ARG @@count:word
	USES edx,ecx,eax, edi, esi
	
	movzx ecx, [@@count]
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
	
	cld
	mov esi, offset buffer
	mov edi, VMEMADR
	mov ecx, FRAMESIZE  
	rep movsb
	cmp [startGame],0
	je @@eind
	call drawScore
	@@eind:
	ret
ENDP time

; -------------------------------------------------------------------
; procedure to check if space bar/escape is tapped
PROC readKey
	USES eax
	mov al, 0
	mov al, [__keyb_rawScanCode] 		; last pressed key
	cmp al, 39h							;space bar
	jne @@checkEscape
	mov [time_counter],0	
	mov eax, [f.y]
	mov [latestY],eax
	@@checkEscape:
	cmp al ,01h							;escape button
	jne @@checkEnter
	call terminateProcess
	@@checkEnter:
	cmp al, 1Ch
	jne @@noPres
	mov [startGame],1
	
			
	@@noPres:
	ret
ENDP readKey

; -------------------------------------------------------------------
;procedure with sthe starting screen to play the game

PROC displayString
	ARG @@offset:DWORD, @@x:dword, @@y:dword
	USES EAX, EBX, EDX
	
	MOV EDX, [@@y] 		; row in EDX
	MOV EBX, [@@x] 		; column in EBX
	MOV AH, 02H			; set cursor position
	SHL EDX, 08H 		; row in DH (00H is top)
	MOV DL, BL 			; column in DL (00H is left)
	MOV BH, 0 			; page number in BH
	INT 10H 			; raise interrupt
	MOV AH, 09H 		; write string to standard output
	MOV EDX, [@@offset] ; offset of ’$’-terminated string in EDX
	INT 21H 			; raise interrupt
	RET
ENDP displayString

; -------------------------------------------------------------------
; ALL DRAWING PROCEDURES
; procedure which calls everything that needs to be drawn (comment: lives and boosts are drawn in the specials procedure)
PROC drawproces
	call drawBackground, offset buffer, offset bgframe
	call drawFlappy, offset flappyframe
	call drawAmountLives
	call drawTube, offset b
	call drawTube, offset b2
	ret
ENDP drawproces

; draw the background (for mode 13h)
PROC drawBackground
	ARG @@destptr:dword, @@packedframe: dword
	USES eax, ebx, ecx, edi 
	
	mov ebx, [@@packedframe]		; source pointer
	mov edi, [@@destptr]			; destination pointer
	
	mov ecx, FRAMESIZE				; amount of pixels in ecx
	@@printPixel:					; each byte of the binfile is one pixel
		add ebx,1
		mov	al,[ebx]
		stosb
		loop @@printPixel
	ret 
ENDP drawBackground

; draw flappy (for mode 13h)
PROC drawFlappy
	ARG @@packedframe:dword
	USES eax, ebx, ecx, edx, edi
	
	mov ebx, [@@packedframe]	; source pointer
	
	mov eax,[f.y]
	mov edx, FRAMEWIDTH
	mul edx
	add eax,[f.x]
	mov edi, offset buffer
	add edi,eax					; destination pointer
	
	mov ecx, FLAPPYHEIGHT
@@screenloop:
	push ecx
	mov ecx, FLAPPYWIDTH
	@@printLine:
		mov	al,[ebx]
		cmp al,22				; we do not want to draw pixels with the value of 22 and 24 because this is the white and grey background of the png
		je @@doNotPrint
		cmp al,24
		je @@doNotPrint
		jmp @@print
		@@doNotPrint:
		inc edi
		jmp @@einde
		@@print:
		stosb
		@@einde:
		add ebx,1
		loop @@printLine
	mov edx, FRAMEWIDTH-FLAPPYWIDTH		; move one row down in the video memory
	add edi, edx
	pop ecx
	loop @@screenloop
	
	ret 
ENDP drawFlappy

; draw life (for mode 13h)
PROC drawLife
	ARG @@packedframe:dword, @@x:dword, @@y:dword		;needs arguments x and y to be able to print the amount of lives you have at the moment in the top right corner 
	USES eax, ebx, ecx, edx, edi 
	
	mov ebx, [@@packedframe]		; source pointer
	
	mov eax,[@@y]
	mov edx, FRAMEWIDTH
	mul edx
	add eax,[@@x]
	mov edi, offset buffer
	add edi,eax						; destination pointer
	
	mov ecx, LIFEHEIGHT
@@screenloop:
	push ecx
	mov ecx, LIFEWIDTH
	@@printLine:
		mov	al,[ebx]
		cmp al,33					;we do not want to draw pixels with the value of 33 and 35 because this is the white and grey background of the png
		je @@doNotPrint
		cmp al,35
		je @@doNotPrint
		jmp @@print
		@@doNotPrint:
		inc edi
		jmp @@einde
		@@print:
		stosb
		@@einde: 
		add ebx,1
		loop @@printLine
	mov edx, FRAMEWIDTH-LIFEWIDTH	; move one row down in the video memory
	add edi, edx
	pop ecx
	loop @@screenloop
	
	ret 
ENDP drawLife

; draw boost (for mode 13h)
PROC drawBoost
	ARG @@packedframe:dword
	USES eax, ebx, ecx, edx, edi 
	
	mov ebx, [@@packedframe]		; source pointer
	mov eax,[boost.y]
	mov edx, FRAMEWIDTH
	mul edx
	add eax,[boost.x]
	mov edi, offset buffer
	add edi,eax						; destination pointer
	
	mov ecx, BOOSTHEIGHT
@@screenloop:
	push ecx
	mov ecx, BOOSTWIDTH
	@@printLine:
		mov	al,[ebx]
		cmp al,41					;we do not want to draw pixels with the value of 41 because this is the white and grey background of the png
		je @@doNotPrint
		jmp @@print
		@@doNotPrint:
		inc edi
		jmp @@einde
		@@print:
		stosb
		@@einde: 
		add ebx,1
		loop @@printLine
	mov edx, FRAMEWIDTH-BOOSTWIDTH		; move one row down in the video memory
	add edi, edx
	pop ecx
	loop @@screenloop
	
	ret 
ENDP drawBoost

; draw tube (for mode 13h)
PROC drawTube
	ARG @@ptr:dword
	USES eax,ebx,ecx,edx,esi,edi
	
	mov ebx, [@@ptr]
	mov edi,offset buffer
	
	; we need to check some conditions of the tube because it is a different drawing proces when we are at the edges
	mov edx,[v]
	cmp [ebx+tube.x],300	;check if we have to increase the width of the tube or not (this is the case when the tube is at the right edge)
	jle @@checkLeftEdge
	add [ebx+tube.b],edx
	@@checkLeftEdge:
	cmp [ebx+tube.x],0     ;check if we have to decrease the width of the tube or not (this is the case when the tube is at the left edge)
	jg @@moving_tube
	mov [ebx+tube.x],0		;to hold it still
	sub [ebx+tube.b],edx
	cmp [ebx+tube.b],edx
	jge @@moving_tube
	mov [ebx+tube.b],1
	mov [ebx+tube.x],319
	call randomTube, ebx		;when we jump to the other side we change all the parameters of the tube
	
	@@moving_tube:
	add edi,[ebx+tube.x]
	mov edx,[ebx+tube.y]
	dec edx
	xor esi,esi
	@@firstPart:					;draw upper part of the tube
		mov ecx,[ebx+tube.b]
		mov al,[ebx+tube.color]
		rep stosb
		mov al,0FAh					
		mov [edi],al				;black border(right)
		sub edi,[ebx+tube.b]
		mov [edi],al				;black border(left)
		add edi, FRAMEWIDTH
		dec edx
		jnz @@firstPart
		cmp esi,1
		je @@endd
		mov ecx,[ebx+tube.b]
		mov al,0FAh
		rep stosb					;upper part of the tube black border
		sub edi,[ebx+tube.b]
		
		mov eax, [ebx+tube.h]				
		mov edx,FRAMEWIDTH
		mul edx
		add edi, eax
		mov ecx,[ebx+tube.b]		
		mov al,0FAh
		rep stosb					;lower part of the tube black border
		sub edi,[ebx+tube.b]
		add edi,FRAMEWIDTH
	@@secondPart:					;draw lower part of the tube
		mov edx, FRAMEHEIGHT
		sub edx, [ebx+tube.y]
		sub edx,[ebx+tube.h]
		;dec edx
		mov esi,1
		jmp @@firstPart
	@@endd:
	ret
ENDP drawTube

; draw amounbt of lives in the right top corner 
PROC drawAmountLives
	USES ecx,ebx
	mov ecx,[lives]
	cmp ecx,0
	je @@no_life
	mov ebx,0
	@@drawing:
		call drawLife,offset lifeframe,ebx,0
		add ebx,20
		loop @@drawing
	@@no_life:
	ret
ENDP drawAmountLives

; -------------------------------------------------------------------
; ALL PROCEDURES WHICH UPDATE POSITIONS OF THE USED ELEMENTS IN THE GAME (flappy, tubes, specials)
; procedure which calls everything of which the postion has to be updated
PROC updatePositions
	call updateFlappy		;update the positions of tubes and flappy
	call specials,offset b
	call specials,offset b2
	call updateTube, offset b
	call updateTube, offset b2

	ret
endp updatePositions

; procedure which updates the position of the flappy
PROC updateFlappy
	USES eax,ebx,edx
	
	mov eax,[g]
	mul [time_counter]
	mul [time_counter]
	xor edx,edx
	mov ebx,3		; for a nice parabolic form
	div ebx
	mov ebx,2
	mul ebx

	
	mov [f.y], eax
	mov eax , [v_y]
	mul [time_counter]
	sub [f.y], eax
	mov eax, [latestY]		;parabolic starts from this position
	add [f.y],eax
	
	cmp [f.y],0
	jg @@notAtUpperLimit
	mov [f.y],0
	@@notAtUpperLimit:
	cmp [f.y],180
	jl @@notAtBottomLimit
	mov [f.y],180
	@@notAtBottomLimit:
	
	ret
ENDP updateFlappy

; procedure which moves the tube 
PROC updateTube
	ARG @@ptr:dword
	USES ebx,eax
	mov ebx,[@@ptr]
	mov eax,[v]
	sub [ebx+tube.x],eax
	ret
ENDP updateTube

; generate a new tube with different parameters when it has reached the left edge and jumps to the right side
PROC randomTube
	ARG @@ptr:dword
	USES eax,ebx,ecx
	mov ebx, [@@ptr]
	
	mov eax,FRAMEHEIGHT
	sub eax,[minimum_h]
	sub eax,30
	call random,eax			;like this the opening will start low enough
	add ecx,3
	mov [ebx+tube.y],ecx

	call random,27
	add ecx,[minimum_h]
	mov [ebx+tube.h],ecx 

	ret
ENDP randomTube

; -------------------------------------------------------------------
; ALL THE PROCEDURES WHICH HAVE TO DO WITH THE SPECIALS
; procedure which deals with generating the specials (the vertically moving tubes, increasing the speed of the game, decreasing the hole size of the tubes, extra lives, boosts)
PROC specials
	ARG  @@ptr:dword
	USES eax,ebx,ecx,edx
	mov ebx,[@@ptr]
	mov eax,[score]
	cmp eax,20				;because when score is below 20 we do not want vertically moving tubes, we also avoid the case were score and thus remainder is 0
	jl @@incSpeedDecHole	; we jump to the next special
	call remainder, eax,20	;the vertically moving tubes van be found in intervals [20,30], [40,60],... in other words the intervals get larger when you go further in the game
	cmp ecx,0
	jne @@nextCheck
	mov [movTubeFlag],1
	mov [b.color],3Dh
	mov [b2.color],3Dh
	@@nextCheck:
	call remainder, eax,30
	cmp ecx,0
	jne @@checkIfMov
	mov [movTubeFlag],0
	mov [b.color],30h
	mov [b2.color],30h
	
	@@checkIfMov:
	cmp [movTubeFlag],0
	je @@incSpeedDecHole
	
	cmp [ebx+tube.y],5		;we check if the tube is not to low if yes than we will make him change direction
	jge @@upperBorderOk
	mov [ebx+tube.tester],0
	
	@@upperBorderOk:
	mov eax,[ebx+tube.y]
	add eax,[ebx+tube.h]
	cmp eax,195					;FRAMEHEIGHT-5
	jle @@lowerBorderOk
	mov [ebx+tube.tester],1		;we check again
	
	@@lowerBorderOk:
	cmp[ebx+tube.tester],0		;if the tester is equal to zero we make the tube go down
	jne @@moveUp
	add [ebx+tube.y],1			;here we make the tube move down
	jmp @@incSpeedDecHole
	
	@@moveUp:					; if the tester is equal to one we make the tube go up
	sub [ebx+tube.y],1			; here we make the tube move up
	
	@@incSpeedDecHole:		;increase speed of the tubes, decrease hole size of the tubes 
	cmp [scoreTester],0			;we use a tester otherwise it will increment the speed each time it calls this function
	jne @@extraLife			;here we want to accelarate the tubes and make tho holes smaller
	cmp [v],8				;max value of the speed
	jg @@extraLife
	cmp [score],10			;because at first the remainder will always be zero
	jl @@extraLife		
	
	call remainder, [score],10	;if the score is multiple of 10 we add speed and make the holes smaller
	cmp ecx,0
	jne @@extraLife
	add [v],1				;we increment the speed
	sub [minimum_h],3		;we make tho hole smaller
	mov [scoreTester],1			; we set this flag to one so the speed doesn't keep increasing during the whole time when the score is divisible by 10
							; we set this flag back to zero when the score increases by 1 (this happens in the score procedure)
							
	@@extraLife:
	cmp  [life.draw],1		;life.draw is a tester to check if a life has to be created
	jne @@noExtraLife
	call drawLife, offset lifeframe, [life.x], [life.y]
	call moveSpecial, offset life
	call isLifeHit, offset life
	@@noExtraLife:
	call random,300
	cmp ecx,298					; so we have a chance of 1/300 of creating a life
	jle @@boostSpecial
	mov [life.draw],1			;going to start moving
	
	
	@@boostSpecial:
	cmp  [boost.draw],1			;boost.draw is a tester to check if a life has to be created
	jne @@checkBoost
	call drawBoost, offset boostframe
	;call drawsquare, offset boost, 5,37h
	
	call moveSpecial, offset boost
	call isBoostHit, offset boost
	
	@@checkBoost:
	cmp [boostOn],1				;we check if our boost is on
	jne @@boostNotOn
	mov [life.draw],0			;we don't want to take any lives when your boost is on
	mov[life.x],305			;in case a life was moving we put it back on 310
	mov eax,[lives]				;check if we don't lose lives
	cmp eax,[boost_life]
	je @@lifeOk
	mov eax, [boost_life]
	mov [lives],eax
	@@lifeOk:
	mov eax,[score]				;check if we stop the boost
	sub eax, [boostScore]
	cmp eax, 4
	jle @@boostNotOn			;this means don't stop the boost yet
	mov [v],5
	mov [boostOn],0
	
	@@boostNotOn:
	call random,700		
	cmp ecx,698
	jle @@endSpecials
	mov [boost.draw],1	
	
	@@endSpecials:
	ret
ENDP specials

;procedure which moves the lives and boosts in a random way (up and down at random)
PROC moveSpecial
	USES eax, ebx, ecx
	ARG @@ptr:dword
	mov ebx,[@@ptr]
	sub [ebx+flappy.x],1
	mov eax,[time_counter]
	inc eax
	call remainder, eax,5		;we only have a chance of changing direction of the timecounter is divisible by 5
	cmp ecx,0
	jne @@noChangev
	call random,100
	mov [vSpecial],-1
	cmp ecx,50 				;so we have a chance of 1/2 of moving up/down 
	jl @@noChangev
	mov [vSpecial],1
	
	@@noChangev:
	cmp [ebx+flappy.y],5			;check if the life is not going to the upper border
	jge @@upperBorderOk
	mov [vSpecial],1
	@@upperBorderOk:
	cmp [ebx+flappy.y],190			;check if the life is not going to the lower border
	jle @@lowerBorderOk
	mov [vSpecial],-1
	@@lowerBorderOk:	
	mov ecx,[vSpecial]
	add [ebx+flappy.y],ecx
	
	ret
ENDP moveSpecial

; procedure which checks if the flappy has hit an extra life/ a boost
PROC isSpecialHit
	ARG @@ptr:dword
	USES eax,ebx
	mov [hit],0
	mov ebx,[@@ptr]
	mov eax, [ebx+flappy.x]		;first we check the x coordinates
	sub eax,[f.x]
	cmp eax,20
	jge @@notTouched
	cmp eax,-12
	jle @@notTouched
	
	mov eax, [ebx+flappy.y]		;now we check the y coordinates
	sub eax,[f.y]
	cmp eax,12
	jge @@notTouched			;the extra life is higher than the flappy
	cmp eax, -20
	jle @@notTouched			;the flappy is higher than the extra life

	mov [hit],1					;this means flappy has hit a special
	mov [ebx+flappy.draw],0		
	mov [ebx+flappy.x],305
	
	@@notTouched:
	cmp [ebx+flappy.x],10			;if the life is at the end we want him to stop drawing the life
	jge @@stillInFrame
	mov [ebx+flappy.x],305
	mov [ebx+flappy.draw],0			;here we stop drawing the life
	
	@@stillInFrame:			
	ret
ENDP isSpecialHit

; checks if the life is hit and execute the changes 
PROC isLifeHit
	USES edx
	call isSpecialHit,offset life
	cmp [hit],1
	jne @@LifeNotHit
	inc [lives]
	@@LifeNotHit:
	ret
ENDP isLifeHit

; checks if the life is hit and execute the changes 
PROC isBoostHit
	USES edx,eax
	call isSpecialHit,offset boost
	cmp [hit],1
	jne @@boostNotHit
	mov eax,[v]
	mov edx,2
	mul edx
	mov [v],eax
	mov edx,[score]
	mov [boostScore],edx
	mov edx,[lives]
	mov [boost_life],edx
	mov [boostOn],1
	
	@@boostNotHit:
	ret
ENDP isBoostHit

; -------------------------------------------------------------------
; ALL THE PROCEDURES WHICH HANDLE THE SCORE
; calculate the score
PROC calculateScore
	ARG @@ptr:dword
	USES eax,ebx,edx
	
	mov ebx,[@@ptr]
	mov eax, [ebx+tube.x]
	mov edx,[f.x]
	sub edx,eax
	cmp edx, [ebx+tube.b]
	jle @@doNotIncrease
	sub edx,[v]		;we shift with v pixels every time, but its greater or equal
	cmp edx,[ebx+tube.b]
	jg @@doNotIncrease
	inc [score]
	mov [checkDead],0		;just check once if dead
	mov [scoreTester],0		;when we increased the score we can check again if we have to increase the speed
		
	@@doNotIncrease:
	ret
ENDP calculateScore

;draw the score
PROC drawScore
	USES edi,edx,eax, ebx, ecx
	mov ah,02h	;set cursor position
	mov bh,00h	;page number
	mov dh,01h	;row
	mov dl,013h	;column
	int 10h
	
	mov eax,[score]
	mov	ebx, 10		; divider
	xor ecx, ecx	; counter for digits to be printed

	; Store digits on stack
	@@getNextDigit:
	inc	ecx         ; increase digit counter
	xor edx, edx
	div	ebx   		; divide by 10
	push dx			; store remainder on stack
	test eax, eax	; {cmp eax, 0} check whether zero?
	jnz	@@getNextDigit

    ; Write all digits to the standard output
	mov	ah, 2h 		; Function for printing single characters.
	@@printDigits:		
	pop dx
	add	dl,'0'      	; Add 30h => code for a digit in the ASCII table, ...
	int	21h            	; Print the digit to the screen, ...
	loop @@printDigits	; Until digit counter = 0.

	ret
ENDP drawScore

; -------------------------------------------------------------------
; check if the flappy hits a tube
PROC testDead
	ARG @@ptr_b:dword, @@x_flap:dword, @@y_flap:dword
	USES eax,ebx,ecx
	cmp [checkDead],0
	jne @@endd				;you already checked if he is dead
	
	mov ebx , [@@ptr_b]
	mov eax,[ebx+tube.x]
	sub eax,[@@x_flap]
	cmp eax, FLAPPYWIDTH	
	jg @@endd
	xor ecx,ecx
	sub ecx,[ebx+tube.b]
	cmp eax, ecx			;this is the width of the buis
	jl @@endd
	
	mov eax,[ebx+tube.y]	;the upper side of the tube
	cmp eax,[@@y_flap]
	jge @@dead
	add eax, [ebx+tube.h]	;lower part of the tube
	mov ebx,[@@y_flap]		;the heigth is also important
	add ebx, FLAPPYHEIGHT
	cmp eax, ebx
	jge @@endd
	
	@@dead:
	sub [lives],1
	mov [checkDead],1		;don't check directly again if he is dead
	
	@@endd:
	ret
ENDP testDead

; -------------------------------------------------------------------
; OPEN, READ AND CLOSE FILE
; opens file, returns file handle in ax
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

; reads chunk to buffer
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

; closes file
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
PROC terminateProcess
	USES eax
	call 	closeFile, offset bghandle
	call 	closeFile, offset flappyhandle
	call 	closeFile, offset lifehandle
	call 	closeFile, offset boosthandle
	call setVideoMode, 03h
	mov	ax,04C00h
	int 21h
	
	ret
ENDP terminateProcess

; -------------------------------------------------------------------
PROC main
	sti
	cld
	
	push ds
	pop	es

	call setVideoMode,13h
	call updateColourPalette,44, offset palette
	;open, read and drow background
	call openFile, offset bgname, offset bghandle
	call readChunk, FRAMESIZE, offset bghandle, offset bgframe
	call drawBackground, offset buffer, offset bgframe
	; open and read the other files (flappy,life,boost) as well
	call openFile, offset flappyname, offset flappyhandle
	call readChunk,  FLAPPYSIZE, offset flappyhandle, offset flappyframe
	call openFile, offset lifename, offset lifehandle
	call readChunk,  LIFESIZE, offset lifehandle, offset lifeframe
	call openFile, offset boostname, offset boosthandle
	call readChunk,  BOOSTSIZE, offset boosthandle, offset boostframe
	;use code to handlle keyboard from examples
	call __keyb_installKeyboardHandler
	
	call time,3											;this part of the code is to display the starting screen
	call displayString, offset startmsg1,0Ch,07h
	call displayString, offset startmsg2,0Bh,08h
	call displayString, offset escmsg,0Dh,09h
	@@again:
	call readKey
	cmp [startGame],1
	je @@again
	
	@@gameLoop:
		inc [time_counter]
		
		;see if if game is over, we do this at first otherwise you can already be dead with the picture of you still being allive
		call testDead,offset b,[f.x],[f.y]
		call testDead,offset b2,[f.x],[f.y]
	
		call drawproces				;draw all the objects
		call updatePositions		;update all the positions
		
		call readKey				; we see if any specific key was pressed
		call calculateScore, offset b
		call calculateScore, offset b2
		
		call time,3	
		
		xor eax, eax				;check if you have any lives left
		cmp [lives],eax
		je @@gameOver
		
		loop @@gameLoop
	
	@@gameOver:
	mov [startGame],0
	call drawBackground, offset buffer, offset bgframe
	call time,3
	call displayString, offset deadmsg1,0Bh,07h
	call displayString, offset deadmsg2,09h,08h
	call displayString, offset escmsg,0Bh,09h
	
	mov [lives],1			;reset values
	mov [b.x],300
	mov [b2.x],140
	mov [b.b],15
	mov [b2.b],15
	mov [b.y], 75
	mov [b2.y],75
	mov [b.h],70
	mov [b2.h],70
	mov [b.color],30h
	mov [b2.color],30h
	
	mov [f.y],100
	mov [latestY],100
	mov [time_counter],0
	
	mov [score],0
	mov [v],4
	mov [checkDead],0
	mov [movTubeFlag], 0
	mov [minimum_h],70		;minimum gap
	mov [scoreTester], 0
	mov [hit],0
	
	mov [life.draw],0
	mov [life.x],300
	mov [boost.draw],0
	mov [boost.x],310

	mov [boost_life],1

	jmp @@again
	
ENDP main

; -------------------------------------------------------------------
UDATASEG		;unitialized data
   	bghandle dw ?
	bgframe db FRAMESIZE dup (?)
	flappyhandle dw ?
	flappyframe db FLAPPYSIZE dup (?)
	lifehandle dw ?
	lifeframe db LIFESIZE dup (?)
	boosthandle dw ?
	boostframe db BOOSTSIZE dup (?)
	
	buffer db FRAMESIZE dup (?)
; -------------------------------------------------------------------
DATASEG
	time_counter dd 0
	timer2 dw 0
	boost_life dd 1

	f flappy <,,>
	life flappy <300,50,>
	boost flappy <305,,>
	
	b tube<,,,,>
	b2 tube<,,140,,>
	
	latestY dd 100
	g dd 1
	v_y dd 7
	movTubeFlag dd 0
	
	lives dd 1
	score dd 0
	
	vSpecial dd 1
	boostScore dd	999
	
	boostOn dd 0
	checkDead dd 0		;you just want to decrease a life once if you touch the tube
	
	minimum_h dd 70		;minimum gap
	v dd 4
	scoreTester dd 0
	hit dd 0
	
	startGame dd 0

	palette 	db 22,24,24
db 37,44,22
db 30,47,63
db 29,56,63
db 46,58,63
db 50,61,63
db 39,58,63
db 54,56,63
db 43,60,63
db 23,41,63
db 34,40,24
db 38,57,19
db 22,33,43
db 22,32,42
db 32,50,20
db 50,63,19
db 19,10,12
db 44,37,17
db 35,24,9
db 50,49,48
db 58,20,16
db 57,58,46
db 59,60,57
db 18,10,14
db 62,62,62
db 28,23,25
db 56,32,5
db 49,43,12
db 41,34,14
db 57,62,53
db 56,31,8
db 52,47,9
db 0,0,0
db 57,57,57
db 61,0,0
db 63,63,63
db 63,20,20
db 63,10,10
db 16,10,4
db 59,44,6
db 42,42,40
db 61,61,61
db 36,27,9
db 40,37,23
	
	bgname 			db "bgalt.bin", 0
	flappyname 		db "flappy.bin", 0
	lifename 		db "life.bin", 0
	boostname 		db "boost.bin", 0
	openErrorMsg 	db "could not open file", 13, 10, '$'
	readErrorMsg 	db "could not read data", 13, 10, '$'
	closeErrorMsg 	db "error during file closing", 13, 10, '$'
	
	startmsg1	db "Pres enter to play", 13, 10, '$'
	startmsg2	db "this beautiful Game", 13, 10, '$'
	deadmsg1	db "What a great run!", 13, 10, '$'
	deadmsg2	db "Pres enter to play again", 13, 10, '$'
	escmsg		db "Pres esc to exit", 13, 10, '$'

	
; -------------------------------------------------------------------
; STACK
; -------------------------------------------------------------------
STACK 100h

END main