.model tiny

CENTER    equ 11 * 80 * 2 + 40 * 2
COMSTRING equ 0080h
VIDSEG    equ 0b800h
LINEBYTES equ 160

.code
org 100h 

start:          mov     di, COMSTRING           ;going to the com str

                cld                             ;go right in string
                call    getoperands

                push    ax                      ;save init value
                mov     ax, VIDSEG              ;use ax for change seg reg
                mov     es, ax                  ;go to video mem
                pop     ax                      ;get init value 

                call    drawframe

                call    addtext

                mov     ax, 4c00h               ;out of prog
	            int     21h

;----------------------------
;Fill registers with operands from com str
;Entry: di - com string address
;Exit:  ax - width,
;       bx - height, 
;       cx - color attr
;       dx - style, if == 0 -> get frame style string (framestyle)
;Destr: cx
;--------------------------
getoperands     proc

                add     di, 02h                 ;skip length and first space

                call    getdecnum               ;width
                mov     ax, bx                  ;to its place

                call    getdecnum               ;height

                push    bx                      ;save init bx value
                call    gethexnum               ;color attr
                mov     cx, bx                  ;get value in cx
                pop     bx                      ;get old bx value

                call    getframestyle           ;get style num and string addr if necessary

                call    getstring               ;get text

                ret
                endp

;----------------------------
;Write dec num in ax
;Entry: di - start string address
;Exit:  di - string address after num and space
;       bx - dec num value
;Destr: ax, si
;--------------------------
getdecnum       proc

                push    ax                      ;save init ax value

                xor     ax, ax                  ;null ax
                xor     bx, bx                  ;null bx
        
                mov     si, di                  ;mov current string addr from di to si for using lodsb
        
nextsymb:       lodsb                           ;get symb
                cmp     al, 20h                 ;cmp with space symb
                je      out                     ;out of operand
        
                sub     al, 30h                 ;make al from char to digit
                push    ax                      ;save digit
                mov     ax, bx                  ;move dig from bx to ax for mul
                xor     bx, bx                  ;null bx because we will use bl for mul 10 (dec)
                mov     bl, 10                  
                mul     bl                      ;ax = al * 10
                mov     bx, ax                  ;move dig from ax to bx, new_bx = old_bx * 10
                pop     ax                      ;get dig
        
                add     bx, ax                  ;add the dig
        
                jmp     nextsymb                ;get new char
        
out:            mov     di, si                  
        
                pop     ax                      ;get init ax value

                ret
                endp

;----------------------------
;Write hex num in ax, gets just two symbs
;Entry: di - start string address
;Exit:  di - string address after num and spaces
;       bx - hex num value 
;Destr: ax
;--------------------------
gethexnum       proc

                push    ax                      ;save init ax value
        
                xor     ax, ax                  ;null ax
                xor     bx, bx                  ;null bx
        
                mov     si, di                  ;mov current string addr from di to si for using lodsb
        
                lodsb                           ;get symb
                cmp     al, 40h                 ;cmp with 40h for find if it gid or alpha
                jb      dig1                    ;less -> digit
        
                sub     al, 07h                 ;sub for alpha, skip line if digit
dig1:           sub     al, 30h                 ;sub for dig and alpha
        
                xor     bx, bx                  ;null bx because we will use bl 
                mov     bl, 10h                 ;for mul 16
                mul     bl                      ;ax = al * 10h
                mov     bx, ax                  
        
                lodsb                           ;get second symb
                cmp     al, 40h                 
                jb      dig2                    
        
                sub     al, 07h                 
dig2:           sub     al, 30h                 
        
                add     bx, ax                  ;add next symb value
        
                mov     di, si                  
        
                pop     ax                      ;get init ax value
        
                ret
                endp

;----------------------------
;Get style num and if it is 0 get style string
;Entry: di - start string address
;Exit:  di - string address after num and spaces
;       dx - frame style num
;Destr: si
;--------------------------
getframestyle       proc

                inc     di                      ;because in gethex we didnt scip space
                push    bx                      ;save init value
                call    getdecnum               ;style num in bx
                mov     dx, bx                  ;get style in dx
                pop     bx                      ;get old bx value

                cmp     dx, 00h                 ;cmp with free style num 
                jne     entend                  ;if equal -> get string      

                mov     si, di                  ;for using movsb

                mov     di, offset framestyle   ;to the reserved for the string place
        
                push    cx
                mov     cx, 09h                 ;amount of symbs for frame style

frsymb:         movsb                           ;bring to si symbs while not space
                loop    frsymb

       	        pop     cx

                mov     di, si                  ;go back to the com line

entend:         ret
                endp

;----------------------------
;Get string from com str
;Entry: di - string address at current symb
;Exit:  di - string address after string and spaces
;Destr: si
;--------------------------
getstring       proc

                mov     si, di
                mov     di, offset framestring

nextch:         movsb                                ;bring to si symbs while not end of 
                cmp     ds:[si], 0Dh                 ;comparing with cr 
                jne     nextch

                mov     es:[di], '$'                 ;add end

                ret
                endp

;----------------------------
;Calculates width of frame by string length
;Entry: 
;Exit:  bx - new frame length
;Destr: 
;--------------------------
calcwid         proc

                push    cx
                call    strlen  
                push    bx
                xor     bx, bx

                mov     al, cl
                mov     bl, 03h                 ;width is 3 times the text size

                mul     bl

                pop     bx
                pop     cx

                push    ax                      ;save init value
                mov     ax, VIDSEG              ;use ax for change seg reg
                mov     es, ax                  ;go to video mem
                pop     ax                      ;get init value 

                ret
                endp

;----------------------------
;Draws definite frame
;Entry: ax - width,
;       bx - height
;       cx - color attr
;       dx - frame style       
;Exit:  None
;Destr: 
;--------------------------
drawframe       proc

                cmp     ax, 00h 
                jne     frame

                call calcwid

frame:          xor     di, di                  ;null di for frame start address
                xchg    ax, dx                  ;now ax - symb, dx - width
                call    findstart

                cmp     ax, 00h                 ;SWITCH BH
                jne     case_2

                mov     si, offset framestyle
                jmp     strfr

case_2:         cmp     ax, 01h                 
                jne     case_3

                mov     al, ':'
                jmp     onesymb

case_3:         cmp     ax, 02h                 
                jne     case_4

                mov     al, 03h
                jmp     onesymb

case_4:         cmp     ax, 03h                 
                jne      case_5

                mov     si, offset framestyle1
                jmp     strfr

case_5:         mov     si, offset framestyle2
                jmp     strfr        

onesymb:        mov     ah, cl                  ;get color attr
                call    onesymbfr
                jmp     frameend

strfr:          mov     ah, cl                  ;get color attr
                call    strframe
                jmp     frameend

frameend:       ret
                endp


;----------------------------
;Draws one symb line
;Entry: di - string address
;       dx - length,
;       ax - frame style symbol 
;Exit:  di - next line address
;Destr: cx
;--------------------------
printline       proc

                lodsb                           ;draw first line, al - first symb in the string
                stosw

                lodsb
                mov     cx, dx                  ;symbs in line cnt
                sub     cx, 02h                 ;for first and last symbs in frame line
                rep     stosw                   ;draw full line with symb by ax

                lodsb 
                stosw

                ret
                endp

;----------------------------
;Draws definite frame by one symb
;Entry: dx - width,
;       bx - height
;       ax - frame style symb  
;       di - frame start address 
;Exit:  ax - frame style (color)
;Destr: cx
;--------------------------
onesymbfr       proc

                push    di
                mov     cx, dx                  ;symbs in line cnt
                rep     stosw                   ;draw full line with symb by ax

                dec     bx                      ;because first line was drawn
                mov     cx, bx
                dec     cx
                pop     di

side:           add     di, LINEBYTES
                push    di
                stosw

                push    cx

                push    ax
                mov     al, 20h                 ;use space for gap
                mov     cx, dx
                sub     cx, 02h                 ;gap symbs amt
                rep     stosw
                pop     ax                      ;init symbtype

                stosw

                push    ax
                mov     ah, 08h 
                stosw
                pop     ax

                pop     cx
                pop     di
                loop    side

                add     di, LINEBYTES
                push    di
                inc     bx

                mov     cx, dx                  ;symbs in line cnt
                rep     stosw                   ;draw full line with symb by ax

                push    ax
                mov     ah, 08h 
                stosw
                pop     ax

                pop     di
                add     di, LINEBYTES
                add     di, 02h

                push    ax
                mov     ah, 08h 

                mov     cx, dx
                rep     stosw
                pop     ax

                ret
                endp



;----------------------------
;Draws definite frame by one symb
;Entry: dx - width,
;       bx - height
;       ax - frame style symb 
;       di - frame start address
;Exit:  ax - frame style char (color)
;Destr: cx
;--------------------------
strframe        proc    
    
                push    di
                call    printline

                dec     bx                      ;because first line was drawn

                mov     cx, bx
                dec     cx
                pop     di
             
difside:        add     di, LINEBYTES
                push    di
                push    cx
                call    printline
                pop     cx

                dec     bx
                cmp     bx, cx
                je      firstshchar
                jmp     sideshchar

firstshchar:    sub     si, 04h 
                push    ax                      ;make shadow, save init char code
                mov     ah, 08h                 ;black background white letters
                lodsb
                stosw
                pop     ax
                add     si, 03h
                jmp     nextside
        
sideshchar:     push    ax                      ;make shadow, save init char code
                mov     ah, 08h                 ;black background white letters
                stosw
                pop     ax
                jmp     nextside

nextside:       inc     bx
                sub     si, 03h                 ;return to the start of frame style string

                pop     di
                loop    difside                 

;LAST LINE
                add     si, 03h                 ;to the last part of frame style string
                add     di, LINEBYTES
                push    di
                call    printline        

                sub     si, 04h
                push    ax
                mov     ah, 08h                 ;black background white letters
                lodsb
                stosw     
                pop     ax

;SHADOW 
                inc     bx                      ;DRAW LINW FUNC

                pop     di 
                add     di, LINEBYTES
                add     di, 02h

                push    ax
                mov     ah, 08h

                call    printline

                pop ax

                ret
                endp

;----------------------------
;Couts start address of frame (in the center of screen)
;Entry: bx - height
;       dx - width
;Exit:  di - address of new line
;Destr: ax, bx, dx
;--------------------------
findstart       proc

                push    ax
                push    bx

                push    dx
                xor     dx, dx
                mov     di, CENTER ;screen center  CONSTS
                mov     ax, bx  
                xor     bx, bx  
                mov     bx, 02h
                div     bl
                mov     ah, 00h
                mov     bx, LINEBYTES
                mul     bx
                sub     di, ax

                pop     dx
                mov     ax, dx
                push    dx
                xor     dx, dx
                mov     bl, 02h
                div     bx
                xor     dx, dx
                mul     bx
                sub     di, ax

                mov     ax, di
                div     bx 
                xor     dx, dx
                mul     bx                      ;get rid of remainder
                xor     dx, dx

                mov     di, ax

                pop     dx
                pop     bx
                pop     ax

                ret
                endp

;----------------------------
;Add text to the center of frame
;Entry: dx - width,
;       bx - height
;       ax - frame style symb 
;Exit:  
;Destr: 
;--------------------------
addtext          proc

                call    strlen  

                push    ax                      ;save init value
                mov     ax, VIDSEG              ;use ax for change seg reg
                mov     es, ax                  ;go to video mem
                pop     ax                      ;get init value 

                push    ax

                mov     ax, cx                  ;bring str length to ax for calculations
                mov     bl, 02h                 ;cout bytes for going left
                div     bl
                mov     ah, 00h                 
                mul     bl
                mov     di, CENTER
                sub     di, ax                  ;go left to word start

                mov     si, offset framestring  ;preparing for printword

                pop     ax

                call    printword

                ret
                endp

;----------------------------
;Gets length of the frame string
;Entry: None
;Exit:  cx - str length
;Destr: ax, di
;--------------------------
strlen        proc

                push    ax
                mov     ax, cs                          ;segment with init address 
                mov     es, ax                          ;out of vidmem
                pop     ax

                push    ax
                push    di                              ;save frame center address

                mov     di, offset framestring

                xor     cx, cx                          ;null cx                    
                dec     cx                              ;max cx

                mov     al, '$'                         ;symb for cmp

                repne   scasb
                neg     cx                     
                sub     cx, 02h                         ;do not write end mark

                pop     di
                pop     ax                            

                ret
                endp

;----------------------------
;Prints text in the center of the frame
;Entry: di - string center address (with offset)
;Exit:  None
;Destr: ax
;--------------------------
printword       proc

newch:          lodsb                                   ;get char, fill al -> whole symb in ax
                stosw                                   ;print the symb in es:[di]
                loop    newch                           ;rep while word

                ret
                endp

framestyle1: db ':-:* *#-#$'
framestyle2: db 05, 05, 05, 04, 20, 04, 06, 06, 06

framestyle:  db 9 dup(?)
framestring: db ?

end             start