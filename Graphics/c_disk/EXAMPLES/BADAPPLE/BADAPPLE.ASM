; -------------------------------------------------------------------
; 80386
; 32-bit x86 assembly language
; TASM
;
; author:	David Blinder
; date:		1/10/2020
; program:	Monochrome video decoder example
; -------------------------------------------------------------------

IDEAL
P386
MODEL FLAT, C
ASSUME cs:_TEXT,ds:FLAT,es:FLAT,fs:FLAT,gs:FLAT

INCLUDE "playwav.inc"

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

; -------------------------------------------------------------------
CODESEG

; wait for vertical blank
proc wait_VBLANK
	USES eax, edx
	mov dx, 03dah
	
	@@VBlank_phase1:
	in al, dx 
	and al, 8
	jnz @@VBlank_phase1
	@@VBlank_phase2:
	in al, dx 
	and al, 8
	jz @@VBlank_phase2
	
	ret 
endp wait_VBLANK
; -------------------------------------------------------------------

; Read next dword from stream in EDX
PROC NextStreamVal
	ARG RETURNS edx
	USES eax
	
	mov eax, [streampos]
	add eax, 4
	cmp eax, BUFFSIZE
	jl @@loadval
	inc [readbusy]		; signal that the video file is being read
	call readChunk, offset streambuffer, BUFFSIZE, [filehandle]
	dec [readbusy]		; signal reading is finished
	btr [readfail], 0	; check if music interrupt was called during file read
	jnc @@noconflict	; do nothing if no conflict happened
	call readMusicPiece	; finish music copy to compensate for failure during interrupt
@@noconflict:
	xor eax, eax
@@loadval:
	mov edx, [offset streambuffer + eax]
	mov [streampos], eax
	ret
ENDP NextStreamVal

; pick value from stream with Exponential-Golomb decoding
; assumes stream bit register is EDX
PROC nextEGvalue
	ARG RETURNS ecx
	USES eax, ebx
	
	xor eax, eax	; EAX will hold output until the end, before putting in ECX, needed for other things
	movzx bx, [bitcounter]	; current bitcounter in BL, BH == #0 bits from previous streamvalue, if applicable
	test edx, edx	; if EDX == 0 then #remaining zeros == bitcounter
	jne @@skipnewval
	mov bh, bl		; how many 0 bits read
	mov bl, 32		; reset bitcounter
	call NextStreamVal	; get next bitpattern in stream
	
@@skipnewval:
	bsr ecx, edx
	sub cl, 31
	neg cl			; count number of (remaining) zeros in EDX
	shl edx, cl		; shift away zeros to the left
	sub bl, cl		; subtract bitcounter with flushed zeros
	add cl, bh		; total number of zero bits counted
	inc cl			; number of bits to be read = #0s + 1  
	cmp bl, cl		; compare EDX bitcount with total bits needed
	jg @@enoughbits
	
; not enough remaining bits in edx
	xchg bl, cl			; CL == remaining bits in EDX, BH == total bits to be read
	shld eax, edx, cl	; shift remaining bits of EDX as first several MSB of total bitpattern in EAX
	sub cl, bl		
	neg cl				; CL == remaining bits to be read
	call NextStreamVal	; get next bitpattern in stream
	mov bl, 32			; update bitcounter

; enough bits left in register EDX
@@enoughbits:
	sub bl, cl				; subract bits to be read from EDX
	mov [bitcounter], bl	; update bitcounter
	shld eax, edx, cl		; shift (remaining) bits of EDX into EAX
	shl edx, cl				; shift away used bits
	
	lea ecx, [eax-1]		; returns in ECX
	ret
ENDP nextEGvalue

; pick value and decode from stream as a Huffman-encoded table value
; assumes stream bit register is EDX
PROC getHuffmanCode
	ARG RETURNS eax
	USES ebx, ecx
	
	xor eax, eax					; clear EAX for correct additions of AX to EBX
	mov ebx, offset huffmantable	; running pointer in Huffman table
	movzx ecx, [bitcounter]			; remaining bits in EDX
@@mainloop:
	mov ax, [ebx]		; read table entry
	btr ax, 15			; test for MSB, clear bit to get jump offset directly if set
	jnc @@foundvalue	; found needed value, result is in AL
	shl edx, 1			; otherwise, jump to new table position based on next EDX bit
	jc @@largejump		; check value of shifted out LSB
	mov eax, 2			; small jump if '0'
@@largejump:
	add ebx, eax		; large jump if '1', encoded in EAX
@@endloop:
	loop @@mainloop		; continue with EDX if remaining bits are present
	call NextStreamVal	; otherwise, load new value in EDX
	mov cl, 32			; reset bitcounter
	jmp @@mainloop	
@@foundvalue:
	mov [bitcounter], cl	; update bitcounter
	ret
ENDP getHuffmanCode

; Clear frame before processing
; Checks whether palette should be flipped
PROC clearFrame
	USES eax, ebx, ecx, edi
	mov edi, offset packedframe
	xor eax, eax				; reset packedframe to 0
	mov ecx, BITSPERFRAME/4
	mov ebx, [flipptr] 
	dec [flipframes + ebx]
	jnz @@noflip				; check whether flip frame has been reached
	add [dword ptr flipptr], 4
	dec eax						; set EAX to 32 1s
@@noflip:
	rep stosd					; fill frame
	ret
ENDP

; Computes part of frame by processing bitstream
; assumes stream bit register is EDX
PROC processFrame
	ARG	@@destptr:dword, @@remblks:dword
	USES eax, ebx, ecx, edi
	mov edi, [@@destptr]; pointer to output
	mov ebx, [@@remblks]; number of remaining blocks in frame to be processed
	mov ecx, [blocknum]	; how many blocks in phase must still be processed
	test ecx, ecx		; check for zero case
	setz al				; enable state flip if zero
	jnz @@skipstateflip
	call nextEGvalue	; if no more blocks left in state, fetch new value in ECX
@@skipstateflip:
	xor al, [blocktype]	; current state is [blocktype] XOR (state just flipped?)
	jnz @@fullblockphase

@@emptyblockphase:	; skip unchanged blocks
	sub ebx, ecx	; subtract number of remaining blocks
	clc				; signal block state in carry flag = 0
	jle @@closingphase	; all remaining blocks empty?
	shl ecx, 3		; #bytes = 8 * skipped processed blocks
	add edi, ecx	; update EDI pointer with 8 bytes per skipped block
	call nextEGvalue; fetch new value in ECX

@@fullblockphase:
	sub ebx, ecx	; are there enough blocks left in frame?
	jg @@skipadjust ; enough blocks are left, do all ECX
	add ecx, ebx	; Only process remainder of frame
@@skipadjust:	
	shl ecx, 3		; 8 bytes per block
@@datablockloop:
	call getHuffmanCode	; get next Huffman code in AL
	stosb				; store in temp frame
	loop @@datablockloop; loop for stream of data blocks	
	test ebx, ebx
	stc					; signal block state in carry flag = 1
	jle @@closingphase	; no more blocks left if EBX <= 0	
	call nextEGvalue	; fetch new value in ECX
	jmp @@emptyblockphase	

@@closingphase:
	setc [blocktype]		; set block state type encoded in carry flag
	neg ebx					; number of blocks is now -EBX
	mov [blocknum], ebx		; store remaining number of blocks
	ret
ENDP processFrame

; Unpack difference frame data, XORing it with existing frame data 
PROC unpackFrame
	USES eax, ebx, ecx, edx, edi, esi
	mov ecx, SCRHEIGHT/8		; column counter
	mov edi, offset unpackedframe
	mov esi, offset packedframe
@@frameloop:
	push ecx
		mov edx, SCRWIDTH/8		; row counter
		@@rowloop:
		xor ebx, ebx			; pointer offset within block
		mov ecx, 8				; block counter
			@@xorblock:
			lodsb				; fetch next byte
			xor [edi+ebx], al	; xor data
			add ebx, SCRWIDTH/8	; next row in block
			loop @@xorblock
		inc edi					; point to next block
		dec edx
		jnz @@rowloop
	add edi, 7*SCRWIDTH/8		; point to next row
	pop ecx
	loop @@frameloop
	ret
ENDP unpackFrame

; transfer frame to video memory (for double buffering)
PROC transferFrame
	USES ecx, edi, esi
	mov edi, VMEMADR
	mov esi, offset unpackedframe
	mov ecx, BITSPERFRAME/4
	rep movsd
	ret
ENDP transferFrame

PROC main
	sti
	cld

	push ds
	pop	es
	
	; open file, read data
	call openFile, offset video_file
	mov [filehandle], ax
	call readChunk, offset framecount, HEADERSIZE, [filehandle]	; read file header
	call readChunk, offset streambuffer, BUFFSIZE, [filehandle]	; initialize stream
	
	; print intro
	call setVideoMode, 03h	
	mov  ah, 09h
	mov  edx, offset introMsg
	int  21h
	
	; wait for keypress
	mov ah, 0
	int 16h

	; set video mode, init registers, start music
	call setVideoMode, 11h	
	mov ecx, [framecount]
	mov edx, [offset streambuffer]
	call startMusic
	
@@animation_loop:
	; split work over two frames
	call clearFrame
	call processFrame, offset packedframe, BLOCKCOUNT/2	+ WORKOFFSET	; process first frame half
	call wait_VBLANK
	
	call processFrame, offset packedframe + BITSPERFRAME/2 + 8*WORKOFFSET, BLOCKCOUNT/2 - WORKOFFSET	; process second frame half
	call unpackFrame
	call wait_VBLANK
	call transferFrame
	
	; check for early termination
	mov	ah, 01h
	int	16h
	cmp	al, 1Bh
	je	@@finish	
	loop @@animation_loop

@@finish:
	call stopMusic
	call closeFile, [filehandle]
	call terminateProcess
ENDP main

; -------------------------------------------------------------------
DATASEG
				
	video_file db "VIDFILE.BIN", 0
	
	introMsg db "Bad Apple!!", 13, 10,\
				"Music: Masayoshi Minoshima", 13, 10,\
				"Vocals: Nomico", 13, 10,\
				"Animation: Anira", 13, 10,\
				"Video decoder: David Blinder", 13, 10, 13, 10,\
				"Don't forget to turn on sound!", 13, 10,\
				"Press any key to start the video.", 13, 10, '$'
	
	bitcounter db 32	; remaining bits from stream in register
	streampos dd 0		; pointer position in stream
	
	flipptr dd 0		; pointer to current frame to be flipped
	blocktype db 1		; type of block currently processed: 0 = empty block, 1 = data block
	blocknum dd 0		; remaining number of blocks of type 'blocktype' to be processed
; -------------------------------------------------------------------
UDATASEG
	filehandle dw ?
	packedframe db BITSPERFRAME dup (?)	; current packed frame data
	unpackedframe db BITSPERFRAME dup (?) ; unpacked frame buffer
	
	framecount dd ?						; number of frames in video
	huffmantable dw HUFFENTRIES dup (?)	; Huffman table data for encoding video difference bytes
	flipframes dd FLIPFRAMECNT dup (?)	; indices of frames that should be negated ("flipped")
	
	streambuffer db BUFFSIZE dup (?)	; partial buffer of bitstream

; -------------------------------------------------------------------
STACK 100h

END main
