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

FRAMECOUNT EQU 288
RAND_A = 1103515245
RAND_C = 12345

; -------------------------------------------------------------------
CODESEG
STRUC flappy
	ball_x dd 160
	ball_y dd 100
ends flappy

STRUC buis
	buis_y dd 75
	buis_h dd 70
	buis_x dd 300
ends buis
	
	




; Set the video mode
PROC setVideoMode
	ARG @@arg:byte
	uses eax
	
	movzx ax, [@@arg]
	int 10h
ret
ENDP setVideoMode


; ; Fill the background (for mode 13h)
PROC fillBackground
	ARG 	@@fillcolor:byte
	USES 	eax, ecx, edi,ebx

	; Initialize video memory address.
	mov	edi, VMEMADR

	; Scan the whole video memory and assign the background colour.
	mov	ecx, SCRWIDTH*SCRHEIGHT
	mov	al,[@@fillcolor]
	rep	stosb


	ret
ENDP fillBackground

Proc drawBorders
	USES ebx, ecx,eax
	mov ebx,0

	mov edx,SCRHEIGHT
	mov edi ,VMEMADR
	loep:				;draw first black part on left then right
		mov ecx ,20
		mov al,00h
		rep stosb
		add edi,SCRWIDTH
		sub edi,20
		dec edx
		jnz loep
		cmp ebx,1
		je endd
	next:
		mov edx,SCRHEIGHT
		mov edi,VMEMADR
		add edi, SCRWIDTH
		sub edi,20
		mov ebx ,1
		jmp loep
	endd:	
	ret
endp drawBorders

proc time
	ARG @@count:word
	USES edx,ecx,eax
	
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
	ret
endp time


proc drawsquare
	ARG @@x:dword, @@y:dword, @@w:dword, @@color:byte
	USES eax,ecx,edx,esi,edi
	
	mov eax,[@@y]
	mov edx, SCRWIDTH
	mul edx
	add eax,[@@x]
	
	
	mov edi, VMEMADR
	add edi,eax		;begin vierkant
	
	mov edx, [@@w]
	mov esi,edx
	horizon:
		mov ecx,edx
		mov al,[@@color]
		rep stosb
		add edi, SCRWIDTH
		sub edi,[@@w]
		dec esi
		jnz horizon
	ret
ENDP drawsquare


proc new_pos
	USES eax,ebx
	
	mov eax,[g]
	mul [time_counter]
	mul [time_counter]
	mov ebx,3
	div ebx
	mov ebx,2
	mul ebx
	
	
	mov [f.ball_y], eax
	mov eax , [v_y]
	mul [time_counter]
	sub [f.ball_y], eax
	mov eax, [latestY]
	add [f.ball_y],eax
	

	ret
endp new_pos

proc drawBuis
	ARG @@x:dword, @@y:dword, @@h:dword,@@b:dword, @@color:byte
	USES eax,ecx,edx,esi,edi
	
	mov edi,VMEMADR
	add edi,[@@x]
	mov edx,[@@y]
	xor esi,esi
	first:
		mov ecx,[@@b]
		mov al,[@@color]
		rep stosb
		add edi, SCRWIDTH
		sub edi,[@@b]
		dec edx
		jnz first
		cmp esi,1
		je last
	second:
		mov eax, [@@h]
		mov edx,SCRWIDTH
		mul edx
		
		add edi, eax
		
		mov edx, SCRHEIGHT
		sub edx, [@@y]
		mov esi,1
		jmp first
	last:
	ret
ENDP drawBuis

Proc new_posBuis
	;ARG @@x:dword, @@y:dword, @@h:dword
	USES eax
	sub [b.buis_x],4
	
	mov eax, [b.buis_x]
	cmp eax,1
	
	jge not_yet
	add [b.buis_x],305

	call randomBuis
		
	not_yet:		;nog geen nieuwe buis tekenen
	ret
endp new_posBuis



;;alemaal om een random variabele te krijgen
PROC rand_init
    USES    eax, ecx, edx

    mov     ah, 02ch        ; Get system time
    int     21h

    mov     ax, dx          ; Use time to generate seed in EAX
    shl     eax, 16
    mov     ax, cx

    mov     ah, 02ah        ; Get system date
    int     21h

    shl     ecx, 16         ; Mangle date into the seed in EAX
    mov     cx, dx
    xor     eax, ecx
    
    mov     [rand_seed], eax

    ret
ENDP rand_init

PROC rand16
    USES    edx
	call rand_init

    mov     eax, [rand_seed]
    mov     edx, RAND_A
    mul     edx
    add     eax, RAND_C
    mov     [rand_seed], eax
	shr		eax, 16
	
	mov [random],eax
	
    ret
ENDP rand16

proc randomBuis
	;ARG @@y:dword,@@h:dword
	
	USES eax,ebx,edx
	;to get where the opening in the buis is
	xor edx, edx
	call rand16
	mov eax,[random]
	mov ebx,130
	div ebx
	movzx eax,dx	;get the remainder
	;add eax,[b.buis_h]
	mov [b.buis_y],eax 

	
	ret
ENDP randomBuis

Proc drawproces
	call	fillBackground, 0Bh
	call drawsquare,[f.ball_x], [f.ball_y],13, 0Eh
	call drawBuis, [b.buis_x],[b.buis_y],[b.buis_h],15,30h

	ret
endp drawproces




PROC terminateProcess
	USES eax
	call setVideoMode, 03h
	mov	ax,04C00h
	int 21h
	ret
ENDP terminateProcess


PROC main
	push DS
	sti
	cld
	
	push ds
	pop	es

	
	;background
	call setVideoMode,13h
	call	fillBackground, 0Eh
	call drawBorders
	
	loup:
		inc [time_counter]
		
		
		call drawproces
		
		call new_pos		;update the positions of buis and flappy
		call new_posBuis, [b.buis_x],[b.buis_y],[b.buis_h]
		
		call drawBorders		;do this as last

		
		mov ah,01h	;kijk of toets ingedrukt
		int 16h
		jz nopres
		pres:
			mov ah,00h
			int 16h
			mov [time_counter],0
			mov eax,[f.ball_y]
			mov [latestY],eax
			
			
		nopres:
		call time,3
		
		loop loup
	
	
	mov	ah,00h
	int	16h
	; (replace by) call	waitForSpecificKeystroke, 001Bh ; keycode for ESC
	
	call terminateProcess
ENDP main

; -------------------------------------------------------------------

UDATASEG		;unitialized data
    rand_seed   dd ?
DATASEG
	palette		db 768 dup (?)
	
	
	
	time_counter dw 0
	
	random dd 15

	f flappy <,>
	b buis<,,>
	
	
	latestY dd 100
	g dd 1
	v_y dd 10
; -------------------------------------------------------------------
; STACK
; -------------------------------------------------------------------
STACK 100h

END main
