
IDEAL
P386
MODEL FLAT, C
ASSUME cs:_TEXT,ds:FLAT,es:FLAT,fs:FLAT,gs:FLAT


CODESEG

start:
     sti            ; set The Interrupt Flag => enable interrupts
     cld            ; clear The Direction Flag

	mov ebx, 8      ;triangle height
    mov ah, 2h      ;regitster for single symbol printing
    mov esi,1       ;start counter

printline:
    dec ebx         ;initial space count is (height -1)
    jz printpiramid ;skip drawing if no spaces left
    mov ecx,ebx     ;space counter
    mov dl,' '      ;space symbol

printspace:
    int 21h
    loop printspace

printpiramid:
    mov ecx, esi ;star counter
    mov dl, '*'

printstar:
    int 21h
    loop printstar
    add esi,2 ;increment with 2* per layer

    mov dl, 0Dh
    int 21h
    mov dl,0Ah
    int 21h

    test ebx, ebx
    jg printline




	; Wait for keystroke and read character.
	mov ah,00h
	int 16h

	; Terminate process with return code in response to a keystroke.
    mov	ax,4C00h
	int 	21h

DATASEG


STACK 100h

END start
