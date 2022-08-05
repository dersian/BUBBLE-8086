; -------------------------------------------------------------------
; 80386
; 32-bit x86 assembly language
; TASM
;
; author:	David Blinder
; date:		3/11/2018
; program:	FPU example: Gaussian Plot
; -------------------------------------------------------------------

IDEAL
P386
MODEL FLAT, C
ASSUME cs:_TEXT,ds:FLAT,es:FLAT,fs:FLAT,gs:FLAT

; Screen constants
VMEMADR EQU 0A0000h	; video memory address
SCRWIDTH EQU 320	; screen witdth
SCRHEIGHT EQU 200	; screen height

; Gaussian curve constants

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

; Wait for a keystroke.
PROC waitForKeystroke
	ARG 	@@key:byte
	USES 	eax
	
	mov	ah,00h
	int	16h
	ret
ENDP waitForKeystroke

; Terminate the program.
PROC terminateProcess
	USES eax
	call setVideoMode, 03h
	mov	ax,04C00h
	int 21h
	ret
ENDP terminateProcess

; -------------------------------------------------------------------

; Compute linearly spaced points
PROC linspace
	ARG @@dest:dword, @@startval:dword, @@endval:dword, @@count:dword
	LOCAL @@temp:dword
	USES ebx, ecx
	
	mov ebx, [@@dest]	; pointer to dest array elements
	mov ecx, [@@count]	; size of array (must be >0)
	fld [@@startval]	; [start] (FPU stack)
	fld [@@endval]		; [end|start]
	fsub st, st(1)		; [end-start|start]
	dec ecx
	mov [@@temp], ecx
	fild [dword ptr @@temp]	; [N-1|end-start|start]
	fdivp				; [(end-start)/(N-1)|start]
	fxch				; [start|(end-start)/(N-1)]
	
@@storeloop:
	fst [dword ptr ebx]	; store element	
	fadd st, st(1)		; increment by step = (end-start)/(N-1)
	add ebx, 4			; point to next element
	loop @@storeloop
	
	fstp [dword ptr ebx]; store final element (since ecx=N-1), pop from stack
	fstp st				; pop last element from stack

	ret
ENDP linspace

; Plot curve of positive function
PROC plotFunc
	ARG @@src:dword, @@scale:dword, @@color:byte
	LOCAL @@temp:dword
	USES eax, ebx, ecx, edx, edi
	
	mov al, [@@color]
	mov ebx, [@@src]
	mov ecx, SCRWIDTH
	mov edi, VMEMADR
	fld [dword ptr @@scale]
@@plotloop:
	push ecx
	fld [dword ptr ebx]
	fmul st, st(1)
	fistp [@@temp]
	mov edx, [@@temp]
	test edx, edx
	jle @@nextiter
	mov ecx, edx
	sub edx, SCRHEIGHT
	jl @@noclip
	xor edx, edx
	mov ecx, SCRHEIGHT
@@noclip:
	imul edx, -SCRWIDTH
	add edx, edi
@@drawloop:
	mov [edx], al
	add edx, SCRWIDTH
	loop @@drawloop
@@nextiter:
	add ebx, 4
	inc edi
	pop ecx
	loop @@plotloop
	
	fstp st
	ret
ENDP plotFunc

; Compute Gaussian curve values
PROC computeGaussian
	ARG @@ref:dword, @@rsig:dword, @@count:dword
	USES ebx, ecx
	
	mov ebx, [@@ref]
	mov ecx, [@@count]
	
	fldl2e				; log2(exp(1))
	fmul [@@rsig]		; c*log2(exp(1))
	fchs				; -c*log2(exp(1))
@@comploop:
	fld [dword ptr ebx]	; x|-c*log2(exp(1))
	fmul st, st			; x^2|-c*log2(exp(1))
	fmul st, st(1)		; -c*x^2*log2(exp(1)) = y
	
	fld1				; 1|y
	fld st(1)			; y|1|y
	fprem				; y%1|1|y
	f2xm1				; 2^(y%1) - 1|1|y
	fadd				; 2^(y%1)|y
	fscale				
	fstp st(1)			; 2^(y%1) * 2^(y/1) = 2^(y%1 + y/1) = 2^y = exp(-c*x^2)
	
	fstp [dword ptr ebx]
	add ebx, 4
	loop @@comploop
	
	fstp st
	ret
ENDP computeGaussian

PROC main
	sti
	cld
	
	push ds
	pop	es

	call	setVideoMode, 13h
	finit	; initialize FPU	
	call 	linspace, offset curvevals, [minval], [maxval], SCRWIDTH	; compute x values
	call 	computeGaussian, offset curvevals, [rsigma], SCRWIDTH		; compute Gaussian function
	call	plotFunc, offset curvevals, [scaleval], 15					; plot resulting function
		
	call 	waitForKeystroke
	call	terminateProcess
ENDP main

; -------------------------------------------------------------------
DATASEG
	minval dd -10.0		; minimum x value
	maxval dd +10.0		; maximum x value
	scaleval dd 200.0	; function scaling value (for plotting)
	rsigma dd 0.075		; reciprocal sigma (for Gaussian: exp[-x^2 * rsigma])

UDATASEG
	curvevals dd SCRWIDTH dup (?)	; function value array


; -------------------------------------------------------------------
; STACK
; -------------------------------------------------------------------
STACK 100h

END main
