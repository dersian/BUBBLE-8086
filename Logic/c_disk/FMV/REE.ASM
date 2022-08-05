; -------------------------------------------------------------------
; 80386
; 32-bit x86 assembly language
; TASM
;
; author:	David Blinder
; date:		23/10/2017
; program:	IDEAL Function calls.
; -------------------------------------------------------------------

IDEAL
P386
MODEL FLAT, C
ASSUME cs:_TEXT,ds:FLAT,es:FLAT,fs:FLAT,gs:FLAT

; -------------------------------------------------------------------
; CODE
; -------------------------------------------------------------------
MISC_OUTPUT      EQU 03c2h
GC_INDEX         EQU 03ceh
SC_INDEX         EQU 03c4h
SC_DATA          EQU 03c5h
CRTC_INDEX       EQU 03d4h
CRTC_DATA        EQU 03d5h
INPUT_STATUS     EQU 03dah
AC_WRITE         EQU 03c0h
AC_READ          EQU 03c1h		
MAP_MASK         EQU   02h
MEMORY_MODE      EQU   04h
UNDERLINE_LOC    EQU   14h
MODE_CONTROL     EQU   17h
HIGH_ADDRESS     EQU   0ch
LOW_ADDRESS      EQU   0dh
LINE_OFFSET      EQU   13h
PEL_PANNING      EQU   13h
CRTC_LINECOMPARE EQU   18h		
CRTC_OVERFLOW    EQU    7h
CRTC_MAXSCANLINE EQU    9h
AC_MODE_CONTROL	 EQU   10h

bits 16

CODESEG		
	


	
PROC main
	sti
	cld



	

	mov ah,0h		; wait for keystroke
    int 16h
	mov	ax,4C00h 	; terminate
	int 21h
ENDP main

; -------------------------------------------------------------------
; DATA
; -------------------------------------------------------------------
DATASEG



; -------------------------------------------------------------------
; STACK
; -------------------------------------------------------------------
STACK 100h

END main
