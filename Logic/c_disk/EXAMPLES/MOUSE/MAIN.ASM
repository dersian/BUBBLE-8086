;=============================================================================
; 32-bit Assembly Example
;
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;
; Copyright (c) 2015, Tim Bruylants <tim.bruylants@gmail.com>
; All rights reserved.
;
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions
; are met:
;
; 1. Redistributions of source code must retain the above copyright notice,
;    this list of conditions and the following disclaimer.
;
; 2. Redistributions in binary form must reproduce the above copyright notice,
;    this list of conditions and the following disclaimer in the documentation
;    and/or other materials provided with the distribution.
;
; 3. Neither the name of the copyright holder nor the names of its
;    contributors may be used to endorse or promote products derived from this
;    software without specific prior written permission.
;
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
; POSSIBILITY OF SUCH DAMAGE.
;
;=============================================================================

IDEAL
P386
MODEL FLAT, C
ASSUME cs:_TEXT,ds:FLAT,es:FLAT,fs:FLAT,gs:FLAT

INCLUDE "mouse.inc"

; compile-time constants (with macros)
VMEMADR EQU 0A0000h	; video memory address
SCRWIDTH EQU 320	; screen witdth
SCRHEIGHT EQU 200	; screen height

;=============================================================================
; CODE
;=============================================================================
CODESEG

; Set the video mode
PROC setVideoMode
	ARG @@VM:byte
	USES eax


	mov ah, 00h
	mov al, [@@VM]
	int 10h

	ret
ENDP setVideoMode

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

PROC terminateProcess
	USES eax
	call setVideoMode, 03h
	mov	ax,04C00h
	int 21h
	ret
ENDP terminateProcess

; ----------------------------------------------------------------------------
; Mouse function
; AX = condition mask causing call
; CX = horizontal cursor position
; DX = vertical cursor position
; DI = horizontal counts
; SI = vertical counts
; BX = button state:
;      |F-2|1|0|
;        |  | `--- left button (1 = pressed)
;        |  `---- right button (1 = pressed)
;        `------ unused
; DS = DATASEG
; ES = DATASEG
; ----------------------------------------------------------------------------
PROC mouseHandler
    USES    eax, ebx, ecx, edx
	
	and bl, 3			; check for two mouse buttons (2 low end bits)
	jz @@skipit			; only execute if a mousebutton is pressed

    movzx eax, dx		; get mouse height
	mov edx, SCRWIDTH
	mul edx				; obtain vertical offset in eax
	sar cx, 1			; horizontal cursor position is doubled in input 
	add ax, cx			; add horizontal offset
	add eax, VMEMADR	; eax now contains pixel address mouse is pointing to
	mov [eax], bl		; change color

	@@skipit:
    ret
ENDP mouseHandler

PROC main
    sti                                 ; Set The Interrupt Flag
    cld                                 ; Clear The Direction Flag

    push    ds
    pop     es

    call    mouse_present
    cmp     eax, 1
    je      @@mouse_present

    mov     ah, 9
    mov     edx, offset msg_no_mouse
    int     21h

@@mouse_present:
	call setVideoMode,13h

    call mouse_install, offset mouseHandler

	call waitForSpecificKeystroke, 001Bh ; keycode for ESC
	call mouse_uninstall
	call terminateProcess
ENDP main

;=============================================================================
; Uninitialized DATA
;=============================================================================
UDATASEG

;=============================================================================
; DATA
;=============================================================================
DATASEG
msg_no_mouse    db 'No mouse found!', 0dh, 0ah, '$'

msg_test    db 'Test $'

;=============================================================================
; STACK
;=============================================================================
STACK 2000h

END main

