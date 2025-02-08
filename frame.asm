.model tiny
.code
org 100h 

start:  mov bx, 0b800h  ;going to videomem
        mov es, bx 

        mov dx, 0h      ;cycle cnt
        mov cx, 0h      ;line cnt
        mov bx, cx      ;symb cnt

        call drawedge

        call drawside

        call drawedge

	mov ax, 4c00h
	int 21h

;----------------------------
;Draw edge of a frame
;Entry: None
;Exit: None 
;Destr: bx
;--------------------------
drawedge        proc

        start1:
                cmp dx, 5h
                jae end1

                mov byte ptr es:[bx], 'A'
	        mov byte ptr es:[bx+1], 11011010b

                inc dx          ;cycle++
		add bx, 2       ;for writing new symb
                jmp start1
        end1: 

	mov dx, 0h              ;null cycle cnt

        inc cx
        mov ax, cx
        mov bx, 160
        mul bx                  ;new line
        mov bx, ax
        ret
        endp

;----------------------------
;Draw side of a frame
;Entry: None
;Exit: None 
;Destr: bx
;--------------------------
drawside        proc
        start_side:	
	        cmp cx, 4h             ;can not add cnt, use old
                jae end_side
                
	        mov byte ptr es:[bx], 'B'
	        mov byte ptr es:[bx+1], 11011010b
	        add bx, 2

                for_start:	
		        cmp dx, 3h
                        jae for_end
                        mov byte ptr es:[bx], 'A'
	                mov byte ptr es:[bx+1], 11011010b

                        inc dx
		        add bx, 2
                        jmp for_start
                for_end: 

                mov dx, 0h

	        mov byte ptr es:[bx], 'B'
	        mov byte ptr es:[bx+1], 11011010b
	        add bx, 2

                inc cx
                mov ax, cx
                mov bx, 160
                mul bx                  ;new line
                mov bx, ax

                jmp start_side
        end_side:

        ret
        endp
        
end     start