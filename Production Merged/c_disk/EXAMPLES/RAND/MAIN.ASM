;=============================================================================
; 32-bit Assembly LES3
;=============================================================================
IDEAL
P386
MODEL FLAT, C
ASSUME cs:_TEXT,ds:FLAT,es:FLAT,fs:FLAT,gs:FLAT

INCLUDE "rand.inc"

;=============================================================================
; CODE
;=============================================================================
CODESEG

; Procedure to print an unsigned integer.
; IN:   32-bit value
; OUT:  -
PROC printInt
    push    ebp
    mov     ebp, esp
    pushad

    mov     eax, [ebp+8]    ; retrieve argument 1

    mov     ecx, 0
    mov     ebx, 10
 doOneDigit:
    xor     edx, edx        ; outcome is same as mov edx, 0
    div     ebx
    push    edx             ; EDX contains one digit, push it to the stack
    inc     ecx             ; count an extra digit
    cmp     eax, 0          ; check if done, or more digits to process
    jnz     doOneDigit

printOneDigit:
    pop     edx             ; retrieve a digit
    add     edx, 48         ; convert to ASCII character
    mov     ah, 2           ; function 02h of int 21h
    int     21h
    dec     ecx             ; decrement the number of digits to print
    jnz     printOneDigit

    popad
    mov     esp, ebp
    pop     ebp
    ret
ENDP printInt

; ================
; Start of program
; ================
PROC main
    sti                 ; set The Interrupt Flag
    cld                 ; clear The Direction Flag

    push    ds
    pop     es

    call    rand_init

    mov     ecx, 100
@@loopke:
    call    rand16
    call    printInt, eax
    mov     ah, 9
    mov     edx, offset newline
    int     21h
    dec     ecx
    jnz     @@loopke

    mov 	eax, 4c00h      ; AH = 4Ch - Exit To DOS
    int 	21h             ; DOS INT 21h
ENDP main

;=============================================================================
; DATA
;=============================================================================
DATASEG

newline     db  0dh, 0ah, '$'

;=============================================================================
; STACK
;=============================================================================
STACK 10000h

END main

