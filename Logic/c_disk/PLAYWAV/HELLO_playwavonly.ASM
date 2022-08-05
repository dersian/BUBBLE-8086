; -------------------------------------------------------------------
; 80386
; 32-bit x86 assembly language
; TASM
;
; author:	David Blinder
; date:		23/10/2017
; program:	IDEAL Function calls.
;
; Niet in edi schrijven, edi houdt het adress bij waar in het ram de samples naar toe moeten
; Genereer eerst een array in het ram met 0,0,0,0,0,0,0,0,0,255,255,255,255,255,255,255
; Vervang "ReadChunk" met in "Readmusicpiece" met een eigen functie (gebruikmakend van dezelfde argumenten)
; 
; Tip: misschien maar 1/2 blokgolven genereer en dan gewoon someren/manipuleren ipv telkens een nieuwe te maken
;
; -------------------------------------------------------------------

IDEAL
P386
MODEL FLAT, C
ASSUME cs:_TEXT,ds:FLAT,es:FLAT,fs:FLAT,gs:FLAT


; compile-time constants (with macros)
VMEMADR EQU 0A0000h	; video memory address
SCRWIDTH EQU 640	; screen width
SCRHEIGHT EQU 480	; screen height
BLOCKCOUNT EQU SCRWIDTH*SCRHEIGHT/64	; number of blocks per frame
WORKOFFSET EQU 800	; offset work done per frame (purely for optimization, optional)
BUFFSIZE EQU 8192	; number of stream bytes kept in buffer
FLIPFRAMECNT EQU 8
HUFFENTRIES EQU 512
HEADERSIZE EQU 4 + 2*HUFFENTRIES + 4*FLIPFRAMECNT
BITSPERFRAME EQU SCRWIDTH * SCRHEIGHT / 8

SB_BASE equ 220h
SB_IRQ equ 7
SB_DMA equ 1
INT_NUMBER equ 0fh
;SND_BUFF_SIZE EQU 8192
SND_BUFF_SIZE EQU 400

ARR_LENGTH equ 400    ; totale lengte van de blokgolf
ARR_PERIOD equ 200   ; 1/2 van de lengte van 1 blokgolf
ARR_ITTERATIONS equ 2 ; 2 = ARR_DEVIDER
; -------------------------------------------------------------------
; CODE
; -------------------------------------------------------------------
CODESEG		

PROC setVideoMode
	ARG 	@@VM:byte
	USES 	eax

	movzx ax,[@@VM]
	int 10h

	ret
ENDP setVideoMode

PROC terminateProcess
	USES eax
	call setVideoMode, 03h
	mov	ax,04C00h
	int 21h
	ret
ENDP terminateProcess

PROC closeFile
	USES eax, ebx, ecx, edx
	ARG	@@handle:word
	movzx ebx, [@@handle]
	mov ah, 3Eh
	int 21h
	
	jnc @@no_error3 ; carry flag is set if error occurs

	call setVideoMode, 03h
	mov  ah, 09h
	mov  edx, offset closeErrorMsg
	int  21h
	
	mov	ah,00h
	int	16h
	call terminateProcess
	
@@no_error3:
	ret
ENDP closeFile
	
PROC disable_irq_7
	in al, 21h
	or al, 10000000b
	out 21h, al
	ret
ENDP disable_irq_7

PROC turn_speaker_on
	mov bl, 0d1h
	call write_dsp
	ret
ENDP turn_speaker_on

PROC turn_speaker_off
	mov bl, 0d3h
	call write_dsp
	ret
ENDP turn_speaker_off

PROC openFile
	ARG	@@filename:dword RETURNS ax
	USES ebx, ecx, edx
	mov al, 0 ; read only
	mov edx, [@@filename]
	mov ah, 3dh
	int 21h
	
	jnc @@no_error2 ; carry flag is set if error occurs

	call setVideoMode, 03h
	mov  ah, 09h
	mov  edx, offset openErrorMsg
	int  21h
	
	mov	ah, 00h
	int	16h
	call terminateProcess
	
@@no_error2:
	ret
ENDP openFile


PROC write_dsp
	mov dx, SB_BASE
	add dl, 0ch
@@busy:
	in al, dx
	or al, al
	js @@busy
	
	mov al, bl
	out dx, al
	ret
ENDP write_dsp

PROC reset_dsp
	mov dx, SB_BASE
	add dl, 6
	
	mov al, 1
	out dx, al
	sub al, al
@@delay:
	dec al
	jnz @@delay
	out dx, al
	
	sub cx, cx
@@empty:
	mov dx, SB_BASE
	add dl, 0eh
	
	in al, dx
	or al, al
	jns @@next_attempt
	
	sub dl, 4
	in al, dx
	cmp al, 0aah
	je @@reset_ok
	
@@next_attempt:	
	loop @@empty
	
@@reset_ok:
	ret
ENDP reset_dsp

PROC stopMusic
	pushad
	;call exit_auto_init
	call disable_irq_7
	call turn_speaker_off
	call closeFile, [sndhandle]
	popad
	ret
ENDP stopMusic

PROC irq_7_handler NOLANGUAGE
	pushad
	bt [readbusy], 0	; verify if file is being read at the moment
	jnc @@goahead		; delay sound stream reading if busy? otherwise go ahead.
	mov [readfail], 1	; signal failure
	jmp @@skipcopy
@@goahead:
	call readMusicPiece
@@skipcopy:
	
	; SB 8-bit ack
	mov dx, SB_BASE + 0eh
	in al, dx
	
	; EOI
	mov al, 20h
	out 20h, al
	
	popad
	iret
ENDP irq_7_handler

PROC install_isr
	mov ax, cs
	mov ds, ax   	; load data segment with code segment (the one we are in now)
	mov ah, 25h 	; Set Interrupt Vector Command
	mov al, INT_NUMBER ; Interrupt to replace
	mov edx, offset irq_7_handler
	int 21h
	ret
ENDP install_isr

PROC enable_irq_7
	in al, 21h
	and al, 01111111b
	out 21h, al
	ret
ENDP enable_irq_7

PROC calculate_sound_buffer_page_offset
	mov eax, offset sound_buffer
	cmp ax, 65536 - SND_BUFF_SIZE
	jbe @@size_ok
	xor ax, ax
	dec ax
	inc eax
@@size_ok:
	mov [buffer_start], eax
	ret
ENDP calculate_sound_buffer_page_offset

PROC program_dma
	mov dx, 0ah ; write single mask register
	mov al, 05h ; disable DMA channel 1
	out dx, al
	
	mov dx, 0ch ; clear byte pointer flip flop
	mov al, 0 ; any value
	out dx, al 
	
	mov dx, 0bh ; write mode register
	mov al, 59h ; auto-init playback
	out dx, al
	
	mov dx, 03h ; channel 1 count
	mov ax, (SND_BUFF_SIZE - 1)
	out dx, al ; low byte
	mov ax, (SND_BUFF_SIZE - 1) / 256
	out dx, al ; high byte
	

	mov dx, 02h ; channel 1 address
	mov eax, [buffer_start]
	out dx, al ; low byte
	mov al, ah
	out dx, al ; high byte
	
	mov dx, 83h ; page register for 8-bit DMA channel 1
	shr eax, 16
	out dx, al
	
	mov dx, 0ah ; write single mask register
	mov al, 01h ; enable DMA channel 1
	out dx, al
	
	ret
ENDP program_dma

PROC set_sampling_rate
	mov bl, 40h ; time constant
	call write_dsp
	mov bl, 0D3h ; ~22KHz
	call write_dsp
	ret
ENDP set_sampling_rate

PROC readMusicPiece
	USES edi
	mov edi, [buffer_start]
	neg [buffhalf]
	jl @@firsthalf
	add edi, SND_BUFF_SIZE/2
@@firsthalf:
	call readChunk, edi, SND_BUFF_SIZE/2, [sndhandle]
	ret
ENDP readMusicPiece

PROC start_playback
	mov bl, 48h ; set block size for 8-bit auto-init mode
	call write_dsp
	mov bx, (SND_BUFF_SIZE / 2 - 1) ; low byte
	call write_dsp
	mov bx, (SND_BUFF_SIZE / 2 - 1) / 256 ; high byte
	call write_dsp
	
	mov bl, 1ch ; start auto-init 8-bit DMA transfer
	call write_dsp
	ret
ENDP start_playback

PROC readChunk
	ARG	@@destptr:dword, @@buffersize:dword, @@handle:word
	USES eax, ebx, ecx, edx
	movzx ebx, [@@handle]
	mov ecx, [@@buffersize]
	mov edx, [@@destptr]
	mov ah, 3fh
	int 21h
	
	jnc @@no_error4 ; carry flag is set if error occurs

	call setVideoMode, 03h
	mov  ah, 09h
	mov  edx, offset readErrorMsg
	int  21h
	
	mov	ah,00h
	int	16h
	call terminateProcess
	
@@no_error4:
	ret
ENDP readChunk

PROC makebloc
	ARG @@temp_eax:dword, @@temp_ebx:dword, @@temp_ecx:dword, @@temp_esi:dword, @@arrdata:dword
	USES eax, ebx, ecx, esi

	mov [temp_eax],eax
	mov [temp_ebx],ebx
	mov [temp_ecx],ecx
	mov [temp_esi],esi

	xor esi, esi
	mov esi,[@@arrdata]

	mov eax, 0          ; counter voor 0/256 [0 = 0 , 1 = 256]
	mov ecx, 0          ; outer loop counter
@@outerLoop:

	cmp ecx, ARR_ITTERATIONS        ;outer, zijn we klaar?
	je @@done
	mov ebx, 0          ; inner counter op 0
    jmp @@innerLoop

@@innerLoop:	
    
	cmp ebx, ARR_PERIOD         ; limiet voor de 2de loop
    je @@innerLoopDone 
	cmp eax, 0
	je @@addzero
	cmp eax, 1
	je @@addtwofivesix
	inc ebx
	jmp @@innerLoop

@@innerLoopDone:
	inc ecx
	cmp eax, 0
	je @@eaxwasnul
	mov eax, 0
	jmp @@outerLoop

@@eaxwasnul: 
	mov eax, 1           ; set eax to 1
	jmp @@outerLoop

@@addzero:
	mov [dword ptr (esi)], 1 ; HEX: F0
	add esi, 4
	inc ebx
	jmp @@innerLoop


@@addtwofivesix:
	mov [dword ptr (esi)], 253 ; HEX: F0
	add esi, 4
	inc ebx
	jmp @@innerLoop

@@done: 	
	mov eax,[temp_eax]
	mov ebx,[temp_ebx]
	mov ecx,[temp_ecx]
	mov esi,[temp_esi]

	ret

ENDP makebloc 


PROC main ;PLAYWAV "startMusic"
	sti
	cld

	push ds
	pop	es

	pushad
	call makebloc, [temp_eax], [temp_ebx], [temp_ecx], [temp_esi], offset arrdata

	call openFile, offset sound_file
	mov [filehandle], ax
	mov [sndhandle], ax

	call reset_dsp	
	call turn_speaker_on
	call install_isr
	
	call enable_irq_7
	call calculate_sound_buffer_page_offset
	call program_dma
	call set_sampling_rate
	
	call readMusicPiece
	call readMusicPiece
	call start_playback
	popad

@@animation_loop:

	mov	ah, 01h
	int	16h
	cmp	al, 1Bh
	je	@@finish	
	loop @@animation_loop

@@finish:
	mov ah,0h		; wait for keystroke
    int 16h
	call stopMusic
	call closeFile, [filehandle]
	call terminateProcess 				;standard termination 
ENDP main

; -------------------------------------------------------------------
; DATA
; -------------------------------------------------------------------
DATASEG
	filehandle dw ?
	sound_file db "BAMUSIC.RAW", 0
	;sound_file db "sine.bin", 0

	openErrorMsg db "File not found.", 13, 10, "Please read README.TXT for instructions", 13, 10, '$'
	readErrorMsg db "Error during file reading", 13, 10, '$'
	closeErrorMsg db "Error during file closing", 13, 10, '$'
	buffhalf db 1	; what buffer half has to be copied
	
	readbusy dw 0	; if busy reading something else
	readfail dw 0 	; signal if interrupt was called during busy time
	
	temp_eax dd 0
	temp_ebx dd 0
	temp_ecx dd 0
	temp_esi dd 0

	arrdata dd ARR_LENGTH dup (?)

; -------------------------------------------------------------------
UDATASEG
	buffer_start dd ?
	sound_buffer db 2 * SND_BUFF_SIZE dup (?) ; sound data buffer
	
	sndhandle dw ?

; -------------------------------------------------------------------
; STACK
; -------------------------------------------------------------------
STACK 100h

END main
