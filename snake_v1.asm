IDEAL
MODEL small
STACK 100h
DATASEG
;__________________Data_________________________
; Flags:
disqualified db 0	  ; 0 if you deid ==> True  
; Varialbes 
; snake 
color dw 02			; 1
transform_shape dw 1		  ; 3 ^,v,>,< 
last_movement dw 0 ; 5
invalid_input dw 0 ; 7
snake_len dw 1   ; 9
new_pos dw ((12*80)+40)*2 ; 11 B
; Apple 
apple_pose dw 0	    ; 13 D
; Snake list 
list_of_positions dw ((12*80)+40)*2	   ; 15 F
dw 1999 dup (0)
;list
color_list dw 01
;_________________________________________________
CODESEG







;________________________________________Zone-4 low level functions______________________________________	
;--------------------------form: create_apple----------------------
;input: None - (push dx)
;output: random place (si)
proc random_generate_0_3998
push bp 
mov bp, sp 
push es  ; mess with es 
	push ax  ; Mathematical operations
	push si  ; Multiplier  
	push dx  ; holds the dosbox clock, and the Module = answer 
    push cx	 ; black box

			
			;------------------
			mov ah, 0 ; takes the value in dosbox clock and put it in dx and cx 
			int 1ah
			;------------------

			mov ax, dx	; ax = clock time 

			; add xor with random data for full random:
			mov bx, cx
			xor ax, [bx]
			;------------------
			mov si, 2
			mul si    	; ax = clock number even 
			;------------------
			
			;------------------
			mov si, 4000 
			div si    ; dx = module of 4,000.
			;------------------
			
			mov [bp + 4], dx ; dx = even number between 0 to 3998

	pop cx 
	pop dx
	pop si
	pop ax
pop es
pop bp 
ret 
endp random_generate_0_3998
;-------------------------------------------
;--------------from: initial----------------
proc clear_screen
push di ; hold clear position 
push cx ; loop counter 
push ax ; draw properties

	mov di, 0
	mov cx, 3fffh ; half the positions on the screen 
	;--------------------------------
		loopp:
		;---------------
		mov al, ' '
		mov ah, 0
		mov[es:di], ax
		;---------------
		add di, 2	 ; every poxle is to palces, thus +2.
		loop loopp
	;--------------------------------
pop ax
pop cx	
pop di
ret
endp clear_screen
;-------------------------------------------
;________________________________________________________________________________________________________












;________________________________________Zone-3 mid level functions______________________________________

;________________________________________from: graphical_update__________________________________________
;-------------------create apple-------------------
; input - pointer apple_pose (+4)
; output - none 
proc creat_apple
push bp 
mov bp, sp 
	push bx ; the atribute in the random place 
	push si ; random palce in the textul screen 

		reganerate:
		;---------------
		push bx 
		call random_generate_0_3998 ; 
		pop si 
		;---------------

		;---------------
		mov bx, [es:si]      ; make dx to have the atribute of the si cell 
		cmp bl, " " 
		jne reganerate       ; if the cell is full, Than rganerate random number  
		;---------------
		
		;------------------------------
		mov bl, 162	; the apple simbol
		mov bh, 04 
		mov[es:si], bx ; make the apple position si and draw it.
		;----------------------------
		mov bx, [bp + 4] 
		mov [word ptr bx], si ; make the apple position si 

	pop si 
	pop bx 
pop bp 
ret 2
endp creat_apple
;-------------------------------------------------------------
;----------------------shift-----------------------
; input: the_new_place (+8), offset list_of_positions (+6), [snake_length] (+4)
; output: None
proc shift
push bp
mov bp, sp
	push cx		; number of loops we go to the list
	push bx		; index of positions
	push ax		; transference register

			;-----------------
			mov bx, [bp + 4] ; snake_length
			mov cx, [bp + 4] 
			;------------------

			shl bx, 1
			add bx, [bp + 6]  ; bx = last list index 
            cmp cx, 0 
            je not_shifted
			;------------------
			go_backwords:	; the loop goes from the tail to the head and update the positions
				sub bx, 2
				mov ax, [bx]
				mov [bx + 2], ax 
			loop go_backwords
			;------------------

			mov ax, [bp + 8] ; add the new place in the first place in the list 
			mov [bx], ax
            not_shifted:
	pop ax
	pop bx  ; black box
	pop cx
pop bp 
ret 6
endp shift
;---------------------------------------------------
;-------------------------redraw_snake----------------------------
; input: snake_len(+8), p list_of_positions(+6), p color_list(+4)
proc redraw_snake
push bp
mov bp, sp
	push bx  
	push cx  
    push ax 
    push di
    push si 
        ;---------------
        mov cx, [bp + 8] ; cx = snake_len 
	    dec cx 
        mov bx, [bp + 6] ; bx = p list_of_positions 
        mov di, [bp + 4] ; di = p color_list
        ;---------------

            ;-------draw head------- 
            mov al, 09
            mov ah, [byte ptr transform_shape]
            mov si, [bx]
            mov [es:si], ax
            add bx, 2
            ;------------------------
            cmp [snake_len], 1
            je no_body
        ;____________________________________
        draw_body:

            mov al, 09 					;[byte ptr transform_shape];09;
            mov ah, [byte ptr di]       ; acording to the color_list 
            mov si, [bx]
            mov [es:si], ax

            add bx, 2
            add di, 2
            loop draw_body 
        ;____________________________________
        no_body:

            ;-----delet pixle---------
            mov al, ' '
            mov ah, 0
            mov si, [bx]
            mov [es:si], ax
            ;------------------------

    pop si 
    pop di 
    pop ax 
	pop cx
	pop bx 
pop bp 
ret 6
endp redraw_snake
;---------------------------------------------------------------------------------------
;_______________________________________________________________________________________________________

;_____________________________________from: discualfied_update__________________________________________
;----------------
proc touch_edges
push bp 
mov bp, sp 
push ax
push bx 
push cx 
	;------------------------
	mov ax, [bp + 6]	; first list of positions value 
	mov bx, [bp + 4]	; p disqualified 
	mov cl, 160 
	;------------------------
	; Up
	cmp ax, 158
	jl touch 
	; Down
	cmp ax,  (24*80)*2 	 
	ja touch 
	; Left
	div cl
	cmp ah, 0
	je touch 
	;Right
	cmp ah, 158
	je touch 

	jmp not_touch
	touch:
	mov [byte ptr bx], 1
	not_touch:
pop cx
pop bx
pop ax 
pop bp
ret 4
endp touch_edges
;----------------
; input: list_of_positions - (+8)(bx), [snake_len](+6)
; , disqualified (+4)
; output: None
;--------------Ate Himself-------------
proc ate_himself
push bp 
mov bp, sp 
	push bx 
	push cx  ; loop counter
	push di 
    push si  ; pointer to the body blocks values
    push dx  ; holds the snake head position

        ;------------------------
        mov bx, [bp + 8]	; bx = poniter list_of_positions
        mov cx, [bp + 6]	; cx = 	[snake_len]
        mov di, [bp + 4]	; di = pointer disqualified
        ;------------------------

            mov dx, [bx]	     ; dx = head position 
            mov si, bx 			 ; si = body pointer 
            add si, 2			 ; pointer body 
            
            shl cx, 1
            add cx, bx 			 ; cx = tail pointer 
        ;____________________________________
        check_list:

            cmp dx, [si]  ;checks if the head is the same coordinates as part body 
            je died
            add si, 2	  ; go to the next coordinates 
    
            cmp si, cx 
            jl check_list
        ;____________________________________

        ;---------------------------
        jmp not_died 
        died:
        mov [byte ptr di], 1
        not_died:
        ;---------------------------

    pop dx
    pop si 
	pop di 
	pop cx
	pop bx 
pop bp 
ret 6
endp ate_himself 
;------------------------------
;----------------delay----------------
; input: None 
; output: None
proc delay
push cx	
	
		mov cx, 0ffffh  
	;-------------------------------- 
	out_loop:
		push cx
		mov cx, 100
		;----------------
		inloop:
			loop inloop
		;----------------
	
		pop cx
		loop out_loop
	;--------------------------------

pop cx	; black box
ret     ; retrun the ip in order to 
endp delay
;--------------------------------------------------
;________________________________________________________________________________________________________














;________________________________________Zone-2 The Three Main Function__________________________________
;_________________F3 = grafical_update___________________
; input: p apple_pose(+14), color_list(+12), p_transform_shape(+10), p_new_pos(+8),
; p_list_of_positions(+6), p_snake_len(+4)
proc graphical_update
push bp 
mov bp, sp 
push bx 
push ax 

	;----------------
	mov bx, [bp + 8]
	push [word ptr bx]	; new_pos 
	;----------------   
	push [bp + 6]		; p list_of_positions 
	;----------------
	mov bx, [bp + 4]
	push [word ptr bx]	; snake len 
	call shift

	;_____________eat apple________________
	mov bx, [bp +6]
	mov ax, [word ptr bx]	; ax = list_of_positions first value
	mov bx, [bp + 14]		; pointer apple_pose 

	cmp ax, [word ptr bx]
	jne not_aten
		mov bx, [bp + 4]
		add [word ptr bx], 1 ; increase snake_len

		;---------------------
		mov bx, [bp + 10] 	; v transform
        push [bx]
        push [bp + 12] 		; offset color_list
		mov bx, [bp + 4]
        push [bx] 			; snake_len 
        call shift    
		;---------------------
        mov bx, [bp + 4]
		push [bp + 14]
		call creat_apple 
		;---------------------
	not_aten:
	;---------------------
	mov bx, [bp + 4]
    push [bx] 			; snake_len 
    push [bp + 6]
	push [bp + 12]
    call redraw_snake  
	;---------------------
pop ax
pop bx 
pop bp 
ret 12
endp graphical_update
;________________________________________________________


;_______________F2 = disqualified_update_________________

proc disqualified_update
push bp
mov bp, sp 
push bx
	
	mov bx, [bp + 8]	; p new position
	;---------------------
	push [word ptr bx] 	; new pos value	
	push [bp + 4]		; p disqualified
	call touch_edges
	;---------------------
	mov bx, [bp + 6]	; p snake_len
	push [bp + 8]		; p list_of_positions
	push [word ptr bx]	; [snake_len]
	push [bp + 4]		; p disqualified
	call ate_himself
	;---------------------
	call delay 
pop bx 
pop bp 
ret 6
endp disqualified_update
;______________________________________________________
;_______________F1 = <move name>_update_________________ 
; input:
	; P_TRANSFORM_SHAPE 10
	; P_LAST_MOVEMENT 8
	; P_INVAILD_INPUT 6
	; P_NEW_POS 4
;--------------------------------------------
proc up_update
push bp 
mov bp, sp
push bx
	mov bx, [bp + 10]
	; add [word ptr bx], 1 	; head color
	mov bx, [bp + 8]
	mov [word ptr bx],  'w'	; last movement 
	mov bx, [bp + 6] 
	mov [word ptr bx],  's' ; invalid input
	mov bx, [bp + 4] 
	sub [word ptr bx],  160 ; snake move
pop bx	
pop bp 
ret 8
endp up_update
;--------------------------------------------
;--------------------------------------------
proc down_update
push bp 
mov bp, sp
push bx 
	mov bx, [bp + 10]
	; add [word ptr bx], 1 	; head color
	mov bx, [bp + 8]
	mov [word ptr bx], 's'	; last movement 
	mov bx, [bp + 6] 
	mov [word ptr bx], 'w' 	; invalid input
	mov bx, [bp + 4] 
	add [word ptr bx], 160 	; snake move
pop bx
pop bp
ret 8
endp down_update
;--------------------------------------------
;--------------------------------------------
proc left_update
push bp 
mov bp, sp
push bx 
	mov bx, [bp + 10]
	; add [word ptr bx], 1 	; head color
	mov bx, [bp + 8]
	mov [word ptr bx], 'a'	; last movement 
	mov bx, [bp + 6] 
	mov [word ptr bx], 'd' 	; invalid input
	mov bx, [bp + 4] 
	sub [word ptr bx], 2 	; snake move
pop bx
pop bp
ret 8
endp left_update
;--------------------------------------------
;--------------------------------------------
proc right_update
push bp 
mov bp, sp
push bx 
	mov bx, [bp + 10]
	; add [word ptr bx], 1 	; head color
	mov bx, [bp + 8]
	mov [word ptr bx], 'd'	; last movement 
	mov bx, [bp + 6] 
	mov [word ptr bx], 'a' 	; invalid input
	mov bx, [bp + 4] 
	add [word ptr bx], 2 	; snake move
pop bx 
pop bp 
ret 8
endp right_update
;_______________________________________________________
;_______________________________________________________________________________________
















;______________________________________Root Zone-1________________________________________

; input:  data pointers: for more details look for define 
; output: None
;--------------------------------------
; define input:
	P_INVALID_INPUT equ [bp + 4]
	P_TRANSFORM_SHAPE equ [bp + 6]
	P_LIST_OF_POSITIONS equ [bp + 8] 
	P_LAST_MOVEMENT equ [bp + 10]
	P_NEW_POS equ [bp + 12]
	P_SNAKE_LEN equ [bp + 14]
	P_APPLE_POSE equ [bp + 16]
	P_DISQUALIFIED equ [bp + 18]
	P_COLOR_LIST equ [bp + 20]
;-------------------------------------------
proc up 
push bp 
mov bp, sp
	cmp al, [byte ptr last_movement]
	je change_head_color
	add [transform_shape], 1
	change_head_color:
	;---------------------
	push P_TRANSFORM_SHAPE 
	push P_LAST_MOVEMENT
	push P_INVALID_INPUT 
	push P_NEW_POS
	call up_update
	;----------------------
	push P_APPLE_POSE
	push P_COLOR_LIST
	push P_TRANSFORM_SHAPE
	push P_NEW_POS
	push P_LIST_OF_POSITIONS
	push P_SNAKE_LEN
	call graphical_update
	;--------------------- 
	;---------------------
	push P_LIST_OF_POSITIONS
	push P_SNAKE_LEN
	push P_DISQUALIFIED
	call disqualified_update
	;----------------------
pop bp 
ret 18
endp up 
;-------------------------------------------

;-------------------------------------------
proc down 
push bp 
mov bp, sp 
	cmp al, [byte ptr last_movement]
	je change_head_color1
	add [transform_shape], 1
	change_head_color1:
	;---------------------
	push P_TRANSFORM_SHAPE 
	push P_LAST_MOVEMENT
	push P_INVALID_INPUT 
	push P_NEW_POS
	call down_update
	;----------------------
	push P_APPLE_POSE
	push P_COLOR_LIST
	push P_TRANSFORM_SHAPE
	push P_NEW_POS
	push P_LIST_OF_POSITIONS
	push P_SNAKE_LEN
	call graphical_update
	;--------------------- 
	;---------------------
	push P_LIST_OF_POSITIONS
	push P_SNAKE_LEN
	push P_DISQUALIFIED
	call disqualified_update
	;----------------------
pop bp 
ret 18
endp down 

;-------------------------------------------
proc left 
push bp 
mov bp, sp
	cmp al, [byte ptr last_movement]
	je change_head_color2
	add [transform_shape], 1
	change_head_color2:
	;---------------------
	push P_TRANSFORM_SHAPE 
	push P_LAST_MOVEMENT
	push P_INVALID_INPUT 
	push P_NEW_POS
	call left_update
	;----------------------
	push P_APPLE_POSE
	push P_COLOR_LIST
	push P_TRANSFORM_SHAPE
	push P_NEW_POS
	push P_LIST_OF_POSITIONS
	push P_SNAKE_LEN
	call graphical_update
	;--------------------- 
	;---------------------
	push P_LIST_OF_POSITIONS
	push P_SNAKE_LEN
	push P_DISQUALIFIED
	call disqualified_update
	;----------------------
pop bp 
ret 18
endp left
;-------------------------------------------

;-------------------------------------------
proc right 
push bp 
mov bp, sp 
	cmp al, [byte ptr last_movement]
	je change_head_color3
	add [transform_shape], 1
	change_head_color3:
	;---------------------
	push P_TRANSFORM_SHAPE 
	push P_LAST_MOVEMENT
	push P_INVALID_INPUT 
	push P_NEW_POS
	call right_update
	;----------------------
	push P_APPLE_POSE
	push P_COLOR_LIST
	push P_TRANSFORM_SHAPE
	push P_NEW_POS
	push P_LIST_OF_POSITIONS
	push P_SNAKE_LEN
	call graphical_update
	;--------------------- 
	;---------------------
	push P_LIST_OF_POSITIONS
	push P_SNAKE_LEN
	push P_DISQUALIFIED
	call disqualified_update
	;----------------------
	
pop bp 
ret 18
endp right
;-------------------------------------------

;---------------------start functions----------------------
; input: list_of_positions(+6), apple_position(+4)
; output: None
proc initial
push bp 
mov bp, sp 
push ax
push di

	mov ax, 0b800h	
	mov es, ax
	
	call clear_screen

	mov di, [bp + 6]
	mov al, 09
	mov ah, 2
	mov[es:di], ax

	push [bp + 4] ; apple position 
	call creat_apple

pop di   
pop ax 
pop bp 
ret 4
endp initial
;---------------------------------------------------------
;_____________________________________________________________________________________________
















;___________________________________________________MAIN___________________________________________________
; --------------------------
start:
	mov ax, @data
	mov ds, ax

; ---------------------------
push [list_of_positions]
push offset apple_pose
call initial
;----------------------------
main_game_loop:
		push offset color_list
		push offset disqualified
		push offset apple_pose
		push offset snake_len
		push offset new_pos
		push offset last_movement
		push offset list_of_positions
		push offset transform_shape
		push offset invalid_input
		input:
		mov ah, 1 ; if there is not a new input it will comtinue with the privews press the auto one
		int 16h 
		je auto_movement

		mov ah, 0 ; put in the al the buffer value  
		int 16h 

		auto_movement:
			
			cmp al, [byte ptr invalid_input] 
			je moved
			cmp al, 'w'
			je up_m
			cmp al, 's'
			je down_m
			cmp al, 'a'
			je left_m
			cmp al, 'd'
			je right_m
			cmp al, 'q'
			je exit
			
			cmp [last_movement],  0
			jne moved	; snake moved before
		jmp main_game_loop

		moved:
		mov al, [byte ptr last_movement]
		jmp input
		

		up_m:
		call up
		jmp exit_check

		down_m:
		call down
		jmp exit_check
		
		left_m:
		call left
		jmp exit_check
		
		right_m:
		call right 
		jmp exit_check

		exit_check:
		cmp [disqualified], 1	
		je exit 
jmp main_game_loop
;__________________________________________________________________________________________________________
exit:
	mov ax, 4c00h
	int 21h
END start

